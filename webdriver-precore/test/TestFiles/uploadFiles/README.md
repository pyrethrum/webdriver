# Upload Test Files

This directory contains dummy test files used for file upload testing in the WebDriver BiDi input demos.

## Important Note

**All files in this directory are actually plain text files**, regardless of their file extensions. They are used purely for testing file upload functionality and do not contain the actual content types that their extensions suggest.

For example:
- `image.jpg` - Plain text file (not an actual JPEG image)
- `document2.pdf` - Plain text file (not an actual PDF document)
- `video.mp4` - Plain text file (not an actual video file)
- `spreadsheet.xlsx` - Plain text file (not an actual Excel spreadsheet)
- etc.

## Purpose

These files are used in the `inputSetFilesDemo` to test:
- Single file uploads
- Multiple file uploads
- Various file extensions
- Clearing file inputs

The file extensions are used to simulate realistic file upload scenarios without requiring actual binary files of each type.