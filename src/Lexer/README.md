# Lexer    
  
A utility to provide **flex** like capabilities for haskell  
  
## Syntax  
  ```
  Definitions  
  %%  
  Rules  
  %%  
  Subroutines  
  ```
  
### Definitions  
  ```
  IDENTIFIER PATTERN  
  IDENTIFIER PATTERN  
  ...  
  ```
  
### Rules  
  ```
  PATTERN SUBROUTINE_RULE  
  PATTERN SUBROUTINE_RULE  
  ...  
  ```
   
### Subroutines  
  ```
  SUBROUTINE  
  ```
  
### VARNAME  
  ```
  Letters [a-z] [A-Z] [0-9] for now  
  Can add other languages later  
  
  * parseIdentifier   
    * check if is alphanumeric only for now  
    * return single token  
  ```
  
### PATTERN  
  ```
  [] * + () - ^ . ? $ " quoted_string valid_letters  
  quoted_string -> "any string literal"  
  valid_letters -> [a-z] [A-Z] [0-9]  
  escaped_characters -> \n (only this used outside)  
  
  * parsePattern  
    * Return token for each character (special or letter)  
    * Return List of tokens  
    * For Special character, take the next character and return if correct  
  ```
  
### SUBROUTINE  
  ```
  CODE  
  CODE  
  ...  
  
  * parseSubroutine  
    * Just put all the subroutine inside a SUBROUTINE token with the code as lexeme till the end   
  ```
  
### SUBROUTINE_RULE    
  ```
  CODE |   
  {  
    SUBROUTINE  
  }  
  
  * parseSubroutineRule  
    * If open brace not present, read only first line  
    * else do parseSubroutine on block till } is encountered or EOL  
    * Put OBrac and CBrac around the token for error handling  
  ```


## Algorithm

