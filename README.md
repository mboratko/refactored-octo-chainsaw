# 631 HW6 Tests

## Using these tests
1. Plop them into your directory
2. Make sure your verif.ml main method is importable. Mine looks something like:

        let _ =
          if Array.length Sys.argv = 2 then
            let filename = Sys.argv.(1) in
            let (pre, cmd, post) = from_file filename in
            if verify pre cmd post then
              (printf "Verification SUCCEEDED.\n%!"; exit 0)
            else
              (printf "Verification FAILED.\n%!"; exit 1)
          else if Array.length Sys.argv = 1
            then print_string "Importing as Library...\n"
          else failwith "Too many arguments"

3. Run `build.sh`
4. Run `tests.d.byte`

## Making new tests

If you feel like sharing these, go ahead and write some tests called
`your_name.ml`. Commit on a fork and issue a pull request w/ the new tests.
Also, shoot me an email so I know to accept the PR. If you want them to run,
just add a `open Your_name` to the top of the `tests.ml` file and it should run.


## Some other things worth mentioning

I have a `helpers.ml` file - this lets you pretty-print things. This helps for
debuggin 'n stuff. I will probably be adding to this as time goes on.
