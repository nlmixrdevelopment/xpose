context('Check read_nm_files')

# Define files to be tested -----------------------------------------------
test_file1 <- c('ID          KA          CL          V           ALAG1       ETA1        ETA2        ETA3        DV          PRED        RES         WRES',
                '  1.1000E+02  4.1052E-01  2.5483E+01  1.4917E+02  2.3223E-01 -4.5845E-02 -3.5313E-03 -2.1460E+00  0.0000E+00 -3.6889E+00  0.0000E+00  0.0000E+00',
                '  1.1000E+02  4.1052E-01  2.5483E+01  1.4917E+02  2.3223E-01 -4.5845E-02 -3.5313E-03 -2.1460E+00 -2.4841E+00 -5.6877E-01 -1.9153E+00 -3.8853E+00')

test_file2 <- c('TABLE NO.  4',
                ' ID          KA          CL          V           ALAG1       ETA1        ETA2        ETA3        DV          PRED        RES         WRES',
                '  1.1000E+02  4.1052E-01  2.5483E+01  1.4917E+02  2.3223E-01 -4.5845E-02 -3.5313E-03 -2.1460E+00  0.0000E+00 -3.6889E+00  0.0000E+00  0.0000E+00',
                '  1.1000E+02  4.1052E-01  2.5483E+01  1.4917E+02  2.3223E-01 -4.5845E-02 -3.5313E-03 -2.1460E+00 -2.4841E+00 -5.6877E-01 -1.9153E+00 -3.8853E+00')

ctrl_file1 <- xpdb_ex_pk$files

# Tests start here --------------------------------------------------------

test_that('message is returned when missing file argument', {
  expect_error(read_nm_files(quiet = TRUE), regexp = 'required')
  expect_error(read_nm_files(quiet = FALSE), regexp = 'required')
})

test_that('message is returned when all provided files are missing', {
  expect_null(read_nm_files(runno = 'run999', quiet = TRUE))
  expect_message(read_nm_files(runno = 'run999', quiet = FALSE), regexp = 'could be found')
})

test_that('read_nm_files handles one file with inappropriate format', {
  file1 <- tempfile(pattern = 'run001', fileext = '.phi')
  file2 <- tempfile(pattern = 'run001', fileext = '.grd')
  file3 <- tempfile(pattern = 'run001', fileext = '.ext')
  on.exit(unlink(c(file1,file2, file3)))
  
  writeLines(text = test_file1, con = file1)
  writeLines(text = test_file2, con = file2)
  writeLines(text = test_file2, con = file3) 
  expect_is(read_nm_files(files = c(file1, file2, file3), quiet = TRUE), class = 'tbl_df')
  expect_message(read_nm_files(files = c(file1, file2, file3), quiet = FALSE), regexp = 'Dropping.+inappropriate format')
})

test_that('read_nm_files handles all files with inappropriate format', {
  file4 <- tempfile(pattern = 'run001', fileext = '.phi')
  file5 <- tempfile(pattern = 'run001', fileext = '.grd')
  file6 <- tempfile(pattern = 'run001', fileext = '.ext')
  on.exit(unlink(c(file4, file5, file6)))
  
  writeLines(text = test_file1, con = file4)
  writeLines(text = test_file1, con = file5)
  writeLines(text = test_file1, con = file6)
  
  expect_null(read_nm_files(files = c(file4, file5, file6), quiet = TRUE))
  expect_message(read_nm_files(files = c(file4, file5, file6), quiet = FALSE), regexp = 'No output file imported')
})

test_that('Files are imported correctly', {
  expect_identical(read_nm_files(runno = '001', quiet = TRUE), ctrl_file1)
})
