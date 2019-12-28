To run the code, use the command
ocaml rpn2.ml

It will take in standard input indefinitely until you input q, to quit the program.

If an error is met, the error message will be logged and the program will continue with the next
expression entered.

test.txt contains the 12 tests I have provided. They must be entered one at a time to standard input.
The results are labeled 1 through 12 as well within result.txt

If a blank line, or any none operator/number is entered, "Unknown token" should be logged.

If dividing by zero is encountered, inf will be printed, the default behavior for ocaml floats.
Single floats entered will evaluate as expressions, printing the float.
