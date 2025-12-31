# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview
This is an R package (`matsunoR`) containing helper functions for data analysis, statistical modeling, and visualization. The package combines R functions with high-performance C++ implementations using Rcpp/RcppArmadillo.

## Development Commands

### Package Development
```bash
# Build and check the package
R CMD build .
R CMD check matsunoR_*.tar.gz

# Install package locally for testing
R CMD INSTALL .

# Generate documentation (if using roxygen2)
R -e "roxygen2::roxygenise()"

# Check package dependencies
R -e "devtools::check()"
```

### Testing and Validation
```bash
# Run examples
R -e "devtools::run_examples()"

# Test specific functions interactively
R -e "source('examples/partitions_example.R')"
R -e "source('examples/signaling_example.R')"
```

## Architecture

### Core Function Categories

1. **Mathematical Partitions** (`R/partitions.R`)
   - Creates and manages real-line partitions with validation
   - Functions: `create_partition()`, `create_partition_equal_lengths()`, `get_interval()`, `get_lengths()`, `get_cutoffs()`

2. **ODE Solving for Signaling Models** (`R/solve_singaling.R`)
   - Numerical solution of signaling game ODEs using deSolve
   - Core function: `solve_signaling_ode()` with automatic gradient computation via numDeriv

3. **Statistical Functions** (`src/functions.cpp`)
   - High-performance C++ implementations using RcppArmadillo
   - Key function: `ptruncnorm_cpp()` for truncated normal distributions

4. **Data Processing Utilities**
   - Panel data handling: `make_bal_panel()`, `create_panel()`
   - Outlier detection: `fun.drop_outlier()`
   - Ranking and preprocessing: `compute_rank()`, `preprocess_discontinuity()`

5. **Visualization** (`R/ggplot_theme_original.R`)
   - Custom ggplot2 theme: `theme_matsuno()` with font integration

### Package Structure
- **R/**: R function implementations
- **src/**: C++ source files with Rcpp bindings
- **man/**: Auto-generated documentation (roxygen2)
- **examples/**: Usage examples for key functions
- **NAMESPACE**: Exported functions (managed by roxygen2)

### Dependencies
- **Core**: Rcpp, RcppArmadillo for C++ integration
- **Numerical**: deSolve for ODE solving, numDeriv for gradients
- **Visualization**: ggplot2
- **Build system**: Uses standard R package tools with Makevars for C++ compilation

### Key Implementation Notes
- The signaling ODE solver uses numerical differentiation when analytical gradients aren't provided
- Partition functions implement strict validation for mathematical correctness (no overlaps/holes)
- C++ functions use log-space calculations for numerical stability
- Font integration expects a global `FONT` variable for theme functions

## Working with C++ Code
When modifying C++ files in `src/`:
```bash
# Recompile C++ code
R -e "Rcpp::compileAttributes()"
R CMD INSTALL .
```

## Example Workflow
```bash
# Typical development cycle
R -e "roxygen2::roxygenise()"  # Update docs
R CMD build .                   # Build package
R CMD check matsunoR_*.tar.gz  # Validate
R CMD INSTALL .                 # Install locally
```