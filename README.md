# Bypher
This is a book cypher program and library made in Haskell to work with text files, with the intention that if needed, analogue decoding is possible.

## Algorithm
The algorithm takes a message to be encoded, and a key text file (acting as the book), from which line numbers and word numbers (on that line) denote the word in the message to be encoded. If a word in the message cannot be found in the key text file, then the program will try to find the individual letters in the word to be encoded, and instead refer to that word as a collection of line numbers and character numbers (on that line). If even then, a needed letter cannot be found, the program will not produce an encoded file.

## Compatible Languages
At the moment this program has only be tested on English texts. However, this program should work for any language that uses spaces to separate words, including most if not all Indo-European languages. With Asian languages, this program can only work with languages that contain spaces, which means that Chinese languages, Japanese, and other languages that do not typically use spaces will be at a disadvantage with this program, since a whole lines will be treated as a single word.

Furthermore, languages that use a high number of inflections will also be at a disadvantage, since it will be harder to find corresponding words in the key text.
