# FIT Test Files

This directory contains sample files with merge conflicts for testing FIT.

## Automated Testing

Run the automated test suite:
```bash
cd test
./run_tests.sh
```

The test suite validates that FIT correctly resolves conflicts in non-interactive mode using the `--incoming`, `--local`, and `--both` flags.

## Non-Interactive Mode

FIT supports non-interactive resolution for automated workflows:

```bash
# Resolve all conflicts with incoming changes
fit file.txt --incoming    # or -i

# Resolve all conflicts with local changes
fit file.txt --local       # or -l

# Resolve all conflicts with both changes
fit file.txt --both        # or -b
```

## Test Files

### Complex Test Files (for automated testing)

#### `multi_conflict.f90.conflict`
**Conflicts:** 4 conflicts
**Best for:** Testing multiple conflicts with varying complexity
Fortran module with conflicts in:
- API exports (function lists)
- Function implementations with different logic
- Overflow handling vs logarithmic computation
- Function presence/absence

```bash
fit test/multi_conflict.f90.conflict
```

#### `similar_conflicts.py.conflict`
**Conflicts:** 4 conflicts
**Best for:** Testing visual distinction of similar content
Python database config with subtle differences:
- Production vs staging hostnames
- Different retry counts
- Similar connection messages

```bash
fit test/similar_conflicts.py.conflict
```

#### `config_conflicts.yaml.conflict`
**Conflicts:** 5 conflicts
**Best for:** Testing minimal differences in config files
YAML configuration with conflicts in:
- Version numbers (2.1.0 vs 2.0.1)
- Port numbers (8080 vs 8000)
- Database hosts and names
- Logging levels and formats
- Cache backends (redis vs memcached)

```bash
fit test/config_conflicts.yaml.conflict
```

**Note:** Test files use the `.conflict` extension to prevent fpm from trying to compile them as part of the build.

### Simple Test Files (for manual testing)

### `quick_test.txt`
**Conflicts:** 2 simple conflicts
**Best for:** Quick testing and demonstrations
Simple text file with straightforward conflicts.

```bash
fit test/quick_test.txt
```

### `simple_fortran.f90`
**Conflicts:** 1 conflict
**Best for:** Testing with Fortran code
A simple Fortran program with one conflict in the calculation logic.

```bash
fit test/simple_fortran.f90
```

### `complex_fortran.f90`
**Conflicts:** 2 conflicts
**Best for:** Testing multiple conflicts in code
A Fortran module with conflicts in matrix operations - shows how FIT handles multiple conflicts in the same file.

```bash
fit test/complex_fortran.f90
```

### `python_example.py`
**Conflicts:** 3 conflicts
**Best for:** Testing with Python code
Python script with conflicts in function definitions and class methods.

```bash
fit test/python_example.py
```

### `documentation.md`
**Conflicts:** 4 conflicts
**Best for:** Testing with documentation/markdown
Markdown documentation with conflicts in installation instructions, examples, and license info.

```bash
fit test/documentation.md
```

## Creating Your Own Test Files

To create a test file with conflicts:

1. Add conflict markers:
   ```
   <<<<<<< HEAD
   incoming changes here
   =======
   local changes here
   >>>>>>> branch-name
   ```

2. Test with FIT:
   ```bash
   fit your-test-file.txt
   ```
   
