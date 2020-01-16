# Schnell-Language
### Using Python-Lex-Yacc
### January 2017

## Setting up the enviroment:
For this project used the following programs:
- Python 2.7.0
- Pyhon-Lex-Yacc 3.0

## Installation steps:
- Go to the following web page and download the Python-Lex-Yacc (PLY).

[Python-Lex-Yacc](https://www.dabeaz.com/ply/)

## Project Description
### Reserved words 
As in any programming language there are **Reserved words** such as: ```If, For, While,``` and more. Schnell covers the following words:

```#Reserved words for Schnell
# For the left side we have how the words have to be written,
# And from the rigth side the type.
reserved = {
   #Begin and end of all the code reserved words.
       'Start' : 'START',
       'EndStart' : 'ENDSTART',
   #Types of variables reserved words.
       'Int' : 'INT',
       'Flo' : 'FLO',
       'Arr1_' : 'Arreglo1',
       'Arr2_' : 'Arreglo2',
   #Staments
        #If reserved words
       'If' : 'IF',
           'True' : 'TRUE',
           'EndTrue' : 'ENDTRUE',
           'False' : 'FALSE',
           'EndFalse' : 'ENDFALSE',
       'EndIf' : 'ENDIF',
       #While reserved words
       'While' : 'WHILE',
       'EndWhile' : 'ENDWHILE',
       #For reserved words
       'For' : 'FOR',
       'EndFor' : 'ENDFOR',
       #Read, Write and Call
       'Read' : 'READ',
       'Printa' : 'PRINTA',
       'Call' : 'CALL',
    #Main reserved words.
       'Main' : 'MAIN',
       'EndMain' : 'ENDMAIN',
    #Method reserved words.
       'Method' : 'METHOD',
       'EndMethod' : 'ENDMETHOD',
   }
```


### Regular Expressions 
The programming language should include simple regular expression such as:
```
# Regular expression rules for simple tokens
t_PLUS    = r'\+'
t_MINUS   = r'-'
t_TIMES   = r'\*'
t_DIVIDE  = r'/'
t_LPAREN  = r'\('
t_RPAREN  = r'\)'
t_LCORCHETE = r'\['
t_RCORCHETE = r'\]'
t_SEMICOLON = r'\;'
t_PUNTOS = r'\:'
t_ASIGNACION = r'\<='
t_IGUALDADCOM = r'\='
t_MAYORCOM = r'\>'
t_MENORCOM = r'\<'
t_COMA = r'\,'
```
### Structure of the some Statements 
- complete structure of a Schnell code looks as follows:
```
def p_Programa(p):
    '''
    Programa : START SAUXI PUNTOS V MethodsBlock SSAUXI MainBlock ENDSTART SEMICOLON SSSAUXI
             | empty
    '''
```
- ```If,For, While``` and more statements structure defined as follows:

```
def p_Statements(p): 
    '''
    Statements : ID ASIGNACION expression SEMICOLON Auxi Statements
               | SVar ASIGNACION expression SEMICOLON AuxiV Statements
               | FOR LPAREN ID ASIGNACION expression COMA FAUXI FFAUXI Comparison FFFAUXI RPAREN PUNTOS Statements FFFFAUXI ENDFOR SEMICOLON Statements
               | WHILE WAUXI LPAREN Comparison WWAUXI RPAREN PUNTOS Statements WWWAUXI ENDWHILE SEMICOLON Statements
               | IF LPAREN Comparison RPAREN IAUXI PUNTOS TRUE PUNTOS Statements ENDTRUE SEMICOLON FALSE PUNTOS IIAUXI Statements ENDFALSE SEMICOLON ENDIF SEMICOLON IIIAUXI Statements
               | IF LPAREN Comparison RPAREN IAUXI PUNTOS TRUE PUNTOS Statements ENDTRUE SEMICOLON ENDIF SEMICOLON IIIAUXI Statements
               | PRINTA LPAREN ID PAUXI RPAREN SEMICOLON Statements
               | PRINTA LPAREN SVar PAUXIV RPAREN SEMICOLON Statements
               | READ LPAREN ID RAUXI RPAREN SEMICOLON Statements
               | READ LPAREN SVar RAUXIV RPAREN SEMICOLON Statements
               | CALL LPAREN ID CAUXI RPAREN SEMICOLON Statements
               | empty
    '''
```

## Project Outcome
The programming language Schnell was able to handle typical operations of a programming language such as: Aritmetic operands, call functions, If statements, For statements, While statements, Main Function, Different type of variables, initialization and declaration of variables, comparision, and arrays. 

## Future Improvements
- Being able to read the code from an external .txt file so we do not have to type the Schnell code on the terminal.
- Optimize the code. 