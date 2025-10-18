#!/bin/bash

# Process test-all.log to create fails.log with only failed tests

input_file="test-all.log"
output_file="fails.log"
workspace_root="/home/john-walker/repos/webdriver/webdriver-precore"

# Clear output file
> "$output_file"

# Read the log file and extract failed tests
in_fail_block=false
current_test_name=""
fail_content=""

while IFS= read -r line; do
  # Detect start of a failed test (line ending with "One of the BiDi client threads failed:" or "FAIL")
  if [[ "$line" =~ ^[[:space:]]+(.*):.*One\ of\ the\ BiDi\ client\ threads\ failed: ]]; then
    # Extract test name (everything before the colon and timing info)
    current_test_name=$(echo "$line" | sed -E 's/^[[:space:]]+(.+):[[:space:]]+(OK|FAIL|One of the BiDi client threads failed:).*/\1/' | sed 's/[[:space:]]*$//')
    in_fail_block=true
    fail_content="$line"$'\n'
    continue
  fi
  
  # Continue collecting failure content
  if [[ "$in_fail_block" == true ]]; then
    fail_content+="$line"$'\n'
    
    # Check if we've reached the end of this failure (line starting with "Use -p")
    if [[ "$line" =~ ^[[:space:]]*Use\ -p ]]; then
      # Process this failure
      # Search for the demo in source files
      
      # Extract module name from test name patterns
      module_name=""
      demo_search=""
      
      if [[ "$current_test_name" =~ ^Browser\ - ]]; then
        module_name="BrowserDemos"
        demo_search=$(echo "$current_test_name" | sed 's/ - EXPECTED ERROR:.*//')
      elif [[ "$current_test_name" =~ ^Network\ [IVX]+ ]]; then
        module_name="NetworkDemos"
        demo_search=$(echo "$current_test_name" | sed 's/:.*//')
      elif [[ "$current_test_name" =~ ^Browsing\ Context\ Events ]]; then
        module_name="BrowsingContextEventDemos"
        demo_search=$(echo "$current_test_name" | sed 's/ - EXPECTED ERROR:.*//')
      elif [[ "$current_test_name" =~ ^Input\ Events ]]; then
        module_name="InputEventDemos"
        demo_search="$current_test_name"
      elif [[ "$current_test_name" =~ ^Network\ Events ]]; then
        module_name="NetworkEventDemos"
        demo_search=$(echo "$current_test_name" | sed 's/ (requires.*$//')
      else
        demo_search="$current_test_name"
      fi
      
      # Search for the demo definition in source files
      if [[ -n "$demo_search" ]]; then
        # Use grep to find the file and line number
        result=$(grep -rn "demo \"$demo_search\"" "$workspace_root/test/BiDi/Demos/" 2>/dev/null | head -1)
        
        if [[ -n "$result" ]]; then
          file_path=$(echo "$result" | cut -d: -f1)
          line_number=$(echo "$result" | cut -d: -f2)
          file_name=$(basename "$file_path")
          module_name=$(basename "$file_name" .hs)
          
          # Create clickable link (VS Code format)
          clickable_link="$file_path:$line_number"
          
          # Write header and content to output
          echo "" >> "$output_file"
          echo "########### $module_name.${demo_search// /_} ###########" >> "$output_file"
          echo "$clickable_link" >> "$output_file"
          echo "" >> "$output_file"
          echo "$fail_content" >> "$output_file"
        fi
      fi
      
      # Reset for next failure
      in_fail_block=false
      current_test_name=""
      fail_content=""
    fi
  fi
done < "$input_file"

echo "Created $output_file with failed tests only"
