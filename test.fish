#!/usr/bin/env fish

argparse 's/state=' 'o/override=' -- $argv 

if test "$_flag_s" = 'y'
  set test_state 0
else
  set test_state 1
end

if test "$_flag_o" = 'y'
  set override 0
else
  set override 1
end


function test_file
  set -l file_path $argv[1]
  set -l out_path "$file_path.out"
  set -l err_path "$file_path.err"
  set -l tmp_out_path "$out_path.tmp"
  set -l tmp_err_path "$err_path.tmp"


  cabal run hasky < "$file_path" 1> $tmp_out_path 2> $tmp_err_path
 
  if test \( ! -f "$out_path" \) -o \( $override -eq 0 \)
    cp -f "$tmp_out_path" "$out_path"
  end

  if test \( ! -f "$err_path" \) -o \( $override -eq 0 \)
    cp -f "$tmp_err_path" "$err_path"
  end

  diff "$out_path" "$tmp_out_path"
  if test ! $status
      printf 'Invalid stdout\n'
      exit 1
  end

  diff "$err_path" "$tmp_err_path"
  if test ! $status
      printf 'Invalid stderr\n'
      exit 1
  end

  rm $tmp_out_path
  rm $tmp_err_path
end

function main
  cabal build hasky
  
  printf '\n\n'

  for file in examples/hasky/**/*.ml
    echo "running $file"
    test_file $file

    if test $status -eq 0
      printf "%s\t OK\n" $file
    else
      printf "%s\t ERROR\n" $file
    end
  end
end

main 
exit $status
