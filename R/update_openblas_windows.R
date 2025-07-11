#' Install or Update OpenBLAS for R on Windows via PowerShell Elevation
#'
#' Downloads the latest x64 OpenBLAS release from GitHub, extracts
#' libopenblas.dll, renames copies to Rblas.dll and Rlapack.dll,
#' and uses an elevated PowerShell prompt (UAC) to replace the
#' existing DLLs in the current R installation's bin/x64 directory.
#'
#' @details
#' Requires an internet connection and the 'httr', 'jsonlite', and 'base64enc'
#' packages. This function will trigger a Windows User Account Control (UAC)
#' prompt requesting administrator privileges to modify files in the R
#' installation directory.
#'
#' @param verbose Logical. If TRUE, prints status messages. Default is TRUE.
#'
#' @return Invisibly returns TRUE on success (PowerShell command sent),
#'         FALSE or stops on failure before attempting elevation. Note that
#'         success here means the elevation prompt was likely shown; it doesn't
#'         guarantee the user approved it or that the copy succeeded within
#'         the elevated process.
#' @export
#'
#' @examples
#' \dontrun{
#' # Run this in a standard (non-admin) R session.
#' # It will trigger a UAC prompt for elevation.
#' update_openblas_windows()
#'
#' # After running, restart R and check sessionInfo()
#' }
update_openblas_windows <- function(verbose = TRUE) {
  # --- 0. Pre-checks ---
  if (.Platform$OS.type != "windows") {
    stop("This function is designed for Windows only.")
  }

  # Check for required packages
  required_pkgs <- c("httr", "jsonlite", "base64enc")
  missing_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]

  if (length(missing_pkgs) > 0) {
    message("The following required packages are missing: ", paste(missing_pkgs, collapse = ", "))
    install_choice <- utils::menu(c("Yes", "No"), title = "Do you want to install them now?")
    if (install_choice == 1) {
      utils::install.packages(missing_pkgs)
      # Re-check after installation attempt
      if (!all(sapply(required_pkgs, requireNamespace, quietly = TRUE))) {
        stop("Failed to install or load required packages. Please install them manually.")
      }
    } else {
      stop("Required packages are missing. Aborting.")
    }
  }

  # Temporary folders and cleanup setup
  if (verbose) message("Setting up temporary directories...")
  temp_dir_base <- file.path(tempdir(), paste0("openblas_update_", Sys.Date()))
  dir.create(temp_dir_base, showWarnings = FALSE, recursive = TRUE)
  tmpzip <- file.path(temp_dir_base, "openblas_download.zip")
  exdir <- file.path(temp_dir_base, "extracted")
  dir.create(exdir, showWarnings = FALSE)

  on.exit(
    {
      if (verbose) message("Cleaning up temporary directory: ", temp_dir_base)
      unlink(temp_dir_base, recursive = TRUE, force = TRUE)
    },
    add = TRUE
  )

  # --- 1. Fetch latest release info ---
  repo <- "OpenMathLib/OpenBLAS"
  api_url <- paste0("https://api.github.com/repos/", repo, "/releases/latest")

  if (verbose) message("Querying GitHub API for the latest OpenBLAS release...")
  response <- tryCatch(
    {
      httr::GET(api_url, httr::user_agent("R-update_openblas_windows-function"))
    },
    error = function(e) {
      stop("Failed to query GitHub API: ", e$message)
    }
  )

  if (httr::http_error(response)) {
    stop("GitHub API request failed with status: ", httr::status_code(response), "\n", httr::content(response, "text"))
  }

  release_info <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))
  latest_version <- release_info$tag_name
  if (verbose) message("Found latest release: ", latest_version)

  # --- 2. Pick the x64 ZIP asset ---
  assets <- release_info$assets
  # Prefer precise match first (ends with x64.zip)
  zip_assets <- assets[grepl("x64\\.zip$", assets$name, ignore.case = TRUE) & assets$content_type == "application/zip", ]
  if (nrow(zip_assets) == 1) {
    asset_url <- zip_assets$browser_download_url[1]
    asset_name <- zip_assets$name[1]
    if (verbose) message("Found asset: ", asset_name)
  } else {
    # Fallback: Allow other potential zip extensions if the primary isn't found
    zip_like_assets <- assets[grepl("x64.*\\.zip", assets$name, ignore.case = TRUE) & grepl("application/(x-)?zip", assets$content_type), ]
    if (nrow(zip_like_assets) >= 1) {
      # Prefer shorter names if multiple matches exist (e.g., prefer .zip over .zip.c7)
      best_match_idx <- which.min(nchar(zip_like_assets$name))
      asset_url <- zip_like_assets$browser_download_url[best_match_idx]
      asset_name <- zip_like_assets$name[best_match_idx]
      if (verbose) message("Found asset (fallback match): ", asset_name)
    } else {
      stop("Could not find a suitable x64 Windows ZIP asset in the latest release.")
    }
  }

  # --- 3. Download & unzip ---
  if (verbose) message("Downloading ", asset_name, "...")
  dl_resp <- httr::GET(asset_url, httr::write_disk(tmpzip, overwrite = TRUE), httr::progress())
  if (httr::http_error(dl_resp)) {
    stop("Failed to download OpenBLAS zip file. Status: ", httr::status_code(dl_resp))
  }
  if (!file.exists(tmpzip)) {
    stop("Download seemed to succeed, but the file is missing: ", tmpzip)
  }
  if (verbose) message("Download complete. Extracting archive...")

  tryCatch(
    {
      utils::unzip(tmpzip, exdir = exdir)
    },
    error = function(e) {
      stop("Failed to extract zip file: ", e$message)
    }
  )
  if (verbose) message("Extraction complete.")

  # --- 4. Find libopenblas.dll ---
  # Search recursively within the extracted directory
  dlls <- list.files(exdir,
    pattern = "^libopenblas\\.dll$",
    recursive = TRUE, full.names = TRUE, ignore.case = TRUE
  )

  if (length(dlls) == 0) {
    stop("libopenblas.dll not found within the extracted archive.")
  }
  if (length(dlls) > 1) {
    warning("Multiple libopenblas.dll found, using the first one: ", dlls[1])
  }
  dll_source_path <- dlls[1]
  if (verbose) message("Found libopenblas.dll at: ", dll_source_path)

  # --- 5. Prepare temporary Rblas.dll & Rlapack.dll ---
  # Create copies in the base temporary directory for easier access
  rblas_temp_path <- file.path(temp_dir_base, "Rblas.dll")
  rlapack_temp_path <- file.path(temp_dir_base, "Rlapack.dll")

  copy_blas_ok <- file.copy(dll_source_path, rblas_temp_path, overwrite = TRUE)
  copy_lapack_ok <- file.copy(dll_source_path, rlapack_temp_path, overwrite = TRUE)

  if (!copy_blas_ok || !copy_lapack_ok) {
    stop("Failed to create temporary Rblas.dll/Rlapack.dll copies.")
  }
  if (verbose) message("Created temporary Rblas.dll and Rlapack.dll.")

  # --- 6. Define Destination ---
  # Target the x64 subdirectory specifically
  dest_dir <- file.path(R.home("bin"))
  if (!dir.exists(dest_dir)) {
    stop("Could not find the R x64 binary directory: ", dest_dir)
  }
  dest_rblas_path <- file.path(dest_dir, "Rblas.dll")
  dest_rlapack_path <- file.path(dest_dir, "Rlapack.dll")

  # --- 7. Construct PowerShell Copy Commands ---
  # Use normalizePath to get absolute paths with correct slashes for PowerShell
  norm_rblas_temp <- normalizePath(rblas_temp_path, winslash = "\\", mustWork = TRUE)
  norm_rlapack_temp <- normalizePath(rlapack_temp_path, winslash = "\\", mustWork = TRUE)
  norm_dest_rblas <- normalizePath(dest_rblas_path, winslash = "\\", mustWork = FALSE)
  norm_dest_rlapack <- normalizePath(dest_rlapack_path, winslash = "\\", mustWork = FALSE)

  if (verbose) {
    message("\nFile paths:")
    message("Source Rblas.dll:   ", norm_rblas_temp)
    message("Source Rlapack.dll: ", norm_rlapack_temp)
    message("Dest Rblas.dll:     ", norm_dest_rblas)
    message("Dest Rlapack.dll:   ", norm_dest_rlapack)
    message("\nVerifying source files exist...")
    message("Rblas.dll exists:   ", file.exists(norm_rblas_temp))
    message("Rlapack.dll exists: ", file.exists(norm_rlapack_temp))
    message("Destination dir exists: ", dir.exists(dirname(norm_dest_rblas)))
  }

  # Simple PowerShell command that just copies the files
  ps_copy_commands <- paste(
    "Write-Host 'Copying OpenBLAS DLLs...';",
    sprintf("Copy-Item -LiteralPath '%s' -Destination '%s' -Force;", norm_rblas_temp, norm_dest_rblas),
    sprintf("Copy-Item -LiteralPath '%s' -Destination '%s' -Force;", norm_rlapack_temp, norm_dest_rlapack),
    "Write-Host 'Copy complete. Please restart R.'",
    sep = " "
  )

  if (verbose) message("\nGenerated PowerShell copy command.")

  # --- 8. Base64 Encode the Command ---
  # Wrap in & { ... } for safety and add error handling within PowerShell
  ps_script_block <- paste0(
    "& {\n",
    "  try {\n",
    "    ", ps_copy_commands, "\n",
    "    Write-Host 'SUCCESS: PowerShell script block completed.'\n",
    "  } catch {\n",
    "    Write-Error \"ERROR in PowerShell script block: $($_.Exception.Message)\"\n",
    "    exit 1\n", # Exit with non-zero status on error
    "  }\n",
    "}"
  )

  # Convert to UTF-16LE directly without using iconv
  encoded_command <- tryCatch(
    {
      # Convert to raw bytes in UTF-16LE
      raw_bytes <- charToRaw(enc2utf8(ps_script_block))
      # Add BOM for UTF-16LE
      bom <- as.raw(c(0xFF, 0xFE))
      # Combine BOM and content
      full_bytes <- c(bom, raw_bytes)
      # Base64 encode
      base64enc::base64encode(full_bytes)
    },
    error = function(e) {
      stop("Error during UTF-16LE encoding: ", e$message)
    }
  )
  if (verbose) message("Base64 Encoded command generated.")

  # --- 9. Construct the Outer PowerShell Command for Elevation ---
  # This command starts an elevated PowerShell instance that executes the encoded command
  ps_elevate_command <- sprintf(
    "Start-Process powershell -Verb RunAs -ArgumentList '-NoProfile', '-ExecutionPolicy', 'Bypass', '-EncodedCommand', '%s'",
    encoded_command
  )
  if (verbose) message("Generated PowerShell elevation command: ", ps_elevate_command)


  # --- 10. Invoke PowerShell to Trigger Elevation ---
  if (verbose) {
    message("--------------------------------------------------------------------")
    message("Attempting to elevate using PowerShell to copy files to:")
    message("  ", normalizePath(dest_dir, winslash = "\\"))
    message("Please approve the Windows UAC prompt that appears.")
    message("The PowerShell window might close quickly on success.")
    message("--------------------------------------------------------------------")
  }

  # Execute the command that triggers Start-Process with RunAs
  # Use system() as system2() quoting with complex args can still be tricky
  # Capture the exit code
  exit_code <- system(paste("powershell -NoProfile -ExecutionPolicy Bypass -Command", shQuote(ps_elevate_command, type = "cmd")))

  # Check the exit code from the initial PowerShell process
  # Note: This reflects whether Start-Process was *launched*, not whether the
  # elevated copy *succeeded*. Success/failure of the copy happens in the
  # separate elevated window.
  if (verbose) {
    message("PowerShell elevation command exit code: ", exit_code)
    if (exit_code != 0) {
      warning("The initial PowerShell process to trigger elevation exited with a non-zero status (", exit_code, "). This might indicate the UAC prompt was cancelled or another issue occurred launching the elevated process.")
    } else {
      message("PowerShell elevation command sent successfully (UAC prompt should have appeared).")
    }
  }

  message("--------------------------------------------------------------------")
  # Cleanup message moved to on.exit
  message("Process initiated. If UAC was approved, files should be copied.")
  message("Please RESTART your R session for the changes to take effect.")
  message("Verify the update using sessionInfo() in the new session.")
  message("--------------------------------------------------------------------")

  return(invisible(exit_code == 0)) # Return TRUE if the initial PS command likely succeeded
}
