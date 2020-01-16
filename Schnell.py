## ------------------------------------------------------------
# Schnell language
# By Andrés Ricardo García Escalante A01375067
# ------------------------------------------------------------
import ply.lex as lex
import ply.yacc as yacc
#Variables globales
global Error # Variable usada para Mostrar la tabla de simbolos en caso que todo este en orden
Error = False
#Listas utilizadas para la tabal de simbolos
NombresTokens = []
ValoresTokens = []
TipoTokens = []
#Listas para el codigo intermedio
Operandos = []
Avail = ["T1","T2","T3","T4","T5","T6","T7","T8","T9","T10","T11","T12","T13","T14","T15","T16","T17","T18","T19","T20","T21","T22","T23","T24","T25","T26","T27","T28","T29","T30"]
AvailAuxi = []
AvailNum = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
AvailBool = [False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False]
Cuadruplos = []
global ContadorCuadruplos
ContadorCuadruplos = 0 #Variable auxiliar.
PilaSaltos=[]
#directorio de procedimientos
NombresProcedimientos = []
CuadruplosProcedimientos = []
#Variables y listas necesarias para la ejecucion del programa
global PC
PC = 1
PCPila = []
PilaEje = []
#Variables dimensionadas
global A
Base = 0
global count
global Cheat
Cheat = 0
count = 0
ArrayGigante =[]
ArrayGiganteTipo =[]
d=[]
m=[]
As=[]
Bases=[]
VariablesDimensionadas = []
VariablesDimensionadasTipo = []
#$Avail
RAvail=["$T1","$T2","$T3","$T4","$T5","$T6","$T7","$T8","$T9","$T10","$T11","$T12","$T13","$T14","$T15","$T16","$T17","$T18","$T19","$T20","$T21","$T22","$T23","$T24","$T25","$T26","$T27","$T28","$T29","$T30"]
RAvailVal=[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]

#Reserved words for Schnell
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

#A regular expression for a Float 
def t_FLOAT(t):
    r'\d+\.\d+'
    t.value = (t.value)
    return t

# A regular expression for a Integer 
def t_INTEGER(t):
    r'\d+'
    t.value = (t.value)
    return t
    
# Define a rule so we can track line numbers
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

# A string containing ignored characters (spaces and tabs)
t_ignore  = ' \t'

# Error handling rule
def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)
    
tokens =  ['ID','INTEGER','FLOAT','PLUS','MINUS','TIMES','DIVIDE','LPAREN','RPAREN','LCORCHETE','RCORCHETE','SEMICOLON','PUNTOS','ASIGNACION','IGUALDADCOM','MAYORCOM','MENORCOM','COMA'] + list(reserved.values())

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value,'ID')    # Check for reserved words
    return t

# Build the lexer
lexer = lex.lex()

# Beginning Yacc
#Gramatica de expresiones numericas.
# Allocation de varialbles a valores
precedence = (
        ('letf','PLUS','MINUS'),
        ('left','TIMES','DIVIDE')
    )

#Estructura de todo el lenguaje
def p_Programa(p):
    '''
    Programa : START SAUXI PUNTOS V MethodsBlock SSAUXI MainBlock ENDSTART SEMICOLON SSSAUXI
             | empty
    '''
#Cuadruplos para el inicio del programa
def p_SAUXI(p):
    '''
    SAUXI : 
    '''
    global ContadorCuadruplos
    ContadorCuadruplos=ContadorCuadruplos+1
    Cuadruplos.append("goto")
    Cuadruplos.append(" ")
    Cuadruplos.append(" ")
    Cuadruplos.append(" ")

def p_SSAUXI(p):
    '''
    SSAUXI : 
    '''
    Cuadruplos[3]=ContadorCuadruplos+1
    
def p_SSSAUXI(p):
    '''
    SSSAUXI : 
    '''
    global ContadorCuadruplos
    Cuadruplos.append("End")
    Cuadruplos.append(" ")
    Cuadruplos.append(" ")
    Cuadruplos.append(" ")
    
    # **********************************************************************************Inicio de la ejecucion*******************************************************************
    #Imprimiendo Cuadruplos (Prueba)
    i=len(Cuadruplos)
    i=i/4
    j=0
    while(i > j):
        k=j*4
        print(j+1,Cuadruplos[k],"   |    ",Cuadruplos[k+1],"   |   ",Cuadruplos[k+2],"    |    ",Cuadruplos[k+3],"    |     ")
        j=j+1
    global Avail
    Avail = ["T1","T2","T3","T4","T5","T6","T7","T8","T9","T10","T11","T12","T13","T14","T15","T16","T17","T18","T19","T20","T21","T22","T23","T24","T25","T26","T27","T28","T29","T30"]
    global PC
    global Cheat
    #Ciclando hasta que se encuentre el cuadruplo End
    while(Cuadruplos[PC*4-4] != "End"):
        a=0
        b=0
        c=0
        d=False
        k=PC*4-4
        #Saltos -------------------------------------------
        if(Cuadruplos[k] == 'goto'):            
            PCPila.append(PC)
            PC = Cuadruplos[k+3]

        #Verificar que esta en el margen-------------------------------------------
        elif(Cuadruplos[k] == 'ver'):
            if(Cuadruplos[k+1] == 'T1' or Cuadruplos[k+1] == 'T2' or Cuadruplos[k+1] == 'T3' or Cuadruplos[k+1] == 'T4' or Cuadruplos[k+1] == 'T5' or Cuadruplos[k+1] == 'T6' or Cuadruplos[k+1] == 'T7' or Cuadruplos[k+1] == 'T8' or Cuadruplos[k+1] == 'T9' or Cuadruplos[k+1] == 'T10' or Cuadruplos[k+1] == 'T11' or Cuadruplos[k+1] == 'T12' or Cuadruplos[k+1] == 'T13' or Cuadruplos[k+1] == 'T14' or Cuadruplos[k+1] == 'T15' or Cuadruplos[k+1] == 'T16' or Cuadruplos[k+1] == 'T17' or Cuadruplos[k+1] == 'T18' or Cuadruplos[k+1] == 'T19' or Cuadruplos[k+1] == 'T20' or Cuadruplos[k+1] == 'T21' or Cuadruplos[k+1] == 'T22' or Cuadruplos[k+1] == 'T23' or Cuadruplos[k+1] == 'T24' or Cuadruplos[k+1] == 'T25' or Cuadruplos[k+1] == 'T26' or Cuadruplos[k+1] == 'T27' or Cuadruplos[k+1] == 'T28' or Cuadruplos[k+1] == 'T29' or Cuadruplos[k+1] == 'T30'):
                Ind1=Avail.index(Cuadruplos[k+1])
                a=AvailNum[Ind1]
            #Buscar en nombre tokens
            else:
                p=0
                j=0
                q=len(NombresTokens)
                while(p<q):
                    if(Cuadruplos[k+1] == NombresTokens[p]):
                        j=1
                        break
                    else:
                        p=p+1
                        
                if(j == 1):
                    Ind1 = p
                    if(TipoTokens[Ind1] == "Int"):
                        a= int(ValoresTokens[Ind1])
                    elif(TipoTokens[Ind1] == "Flo"):
                        a= float(ValoresTokens[Ind1])

                else:
                    a=float(Cuadruplos[k+1])

            if(not((a >= int(Cuadruplos[k+2])) and (a <= int(Cuadruplos[k+3])))):
                print("Lista fuera de rango")
                break
            PC = PC + 1
            Cheat = 1

        # Suma  -------------------------------------------
        elif(Cuadruplos[k] == '+'):
            PCPila.append(PC)
            #Operando A
            #Avail o direcciones valores
            if(Cuadruplos[k+1] == 'T1' or Cuadruplos[k+1] == '$T1' or Cuadruplos[k+1] == 'T2' or Cuadruplos[k+1] == '$T2' or Cuadruplos[k+1] == 'T3' or Cuadruplos[k+1] == '$T3' or Cuadruplos[k+1] == 'T4' or Cuadruplos[k+1] == '$T4' or Cuadruplos[k+1] == 'T5' or Cuadruplos[k+1] == '$T5' or Cuadruplos[k+1] == 'T6' or Cuadruplos[k+1] == '$T6' or Cuadruplos[k+1] == 'T7' or Cuadruplos[k+1] == '$T7' or Cuadruplos[k+1] == 'T8'  or Cuadruplos[k+1] == '$T8' or Cuadruplos[k+1] == 'T9' or Cuadruplos[k+1] == '$T9' or Cuadruplos[k+1] == 'T10' or Cuadruplos[k+1] == '$T10' or Cuadruplos[k+1] == 'T11' or Cuadruplos[k+1] == 'T12' or Cuadruplos[k+1] == 'T13' or Cuadruplos[k+1] == 'T14' or Cuadruplos[k+1] == 'T15' or Cuadruplos[k+1] == 'T16' or Cuadruplos[k+1] == 'T17' or Cuadruplos[k+1] == 'T18' or Cuadruplos[k+1] == 'T19' or Cuadruplos[k+1] == 'T20' or Cuadruplos[k+1] == 'T21' or Cuadruplos[k+1] == 'T22' or Cuadruplos[k+1] == 'T23' or Cuadruplos[k+1] == 'T24' or Cuadruplos[k+1] == 'T25' or Cuadruplos[k+1] == 'T26' or Cuadruplos[k+1] == 'T27' or Cuadruplos[k+1] == 'T28' or Cuadruplos[k+1] == 'T29' or Cuadruplos[k+1] == 'T30' or Cuadruplos[k+1] == '$T11' or Cuadruplos[k+1] == '$T12' or Cuadruplos[k+1] == '$T13' or Cuadruplos[k+1] == '$T14' or Cuadruplos[k+1] == '$T15' or Cuadruplos[k+1] == '$T16' or Cuadruplos[k+1] == '$T17' or Cuadruplos[k+1] == '$T18' or Cuadruplos[k+1] == '$T19' or Cuadruplos[k+1] == '$T20' or Cuadruplos[k+1] == '$T21' or Cuadruplos[k+1] == '$T22' or Cuadruplos[k+1] == '$T23' or Cuadruplos[k+1] == '$T24' or Cuadruplos[k+1] == '$T25' or Cuadruplos[k+1] == '$T26' or Cuadruplos[k+1] == '$T27' or Cuadruplos[k+1] == '$T28' or Cuadruplos[k+1] == '$T29' or Cuadruplos[k+1] == '$T30'):
                if(Cuadruplos[k+1][0] != '$'):
                    Ind1=Avail.index(Cuadruplos[k+1])
                    a=AvailNum[Ind1]

                else:
                    Ind1=RAvail.index(Cuadruplos[k+1])
                    pos=RAvailVal[Ind1]
                    if(ArrayGiganteTipo[pos] == "Int"):
                        a=int(ArrayGigante[pos])
                    else:
                        a= float(ArrayGigante[pos])
                        
            #Buscar en nombre tokens
            else:
                p=0
                j=0
                q=len(NombresTokens)
                while(p<q):
                    if(Cuadruplos[k+1] == NombresTokens[p]):
                        j=1
                        break
                    else:
                        p=p+1
                        
                if(j == 1):
                    Ind1 = p
                    if(TipoTokens[Ind1] == "Int"):
                        a= int(ValoresTokens[Ind1])
                    elif(TipoTokens[Ind1] == "Flo"):
                        a= float(ValoresTokens[Ind1])

                else:
                    a=float(Cuadruplos[k+1])

            #Operando B
            #Avail o direcciones valores
            if(Cuadruplos[k+2] == 'T1' or Cuadruplos[k+2] == '$T1' or Cuadruplos[k+2] == 'T2' or Cuadruplos[k+2] == '$T2' or Cuadruplos[k+2] == 'T3' or Cuadruplos[k+2] == '$T3' or Cuadruplos[k+2] == 'T4' or Cuadruplos[k+2] == '$T4' or Cuadruplos[k+2] == 'T5' or Cuadruplos[k+2] == '$T5' or Cuadruplos[k+2] == 'T6' or Cuadruplos[k+2] == '$T6' or Cuadruplos[k+2] == 'T7' or Cuadruplos[k+2] == '$T7' or Cuadruplos[k+2] == 'T8'  or Cuadruplos[k+2] == '$T8' or Cuadruplos[k+2] == 'T9' or Cuadruplos[k+2] == '$T9' or Cuadruplos[k+2] == 'T10' or Cuadruplos[k+2] == '$T10' or Cuadruplos[k+2] == 'T11' or Cuadruplos[k+2] == 'T12' or Cuadruplos[k+2] == 'T13' or Cuadruplos[k+2] == 'T14' or Cuadruplos[k+2] == 'T15' or Cuadruplos[k+2] == 'T16' or Cuadruplos[k+2] == 'T17' or Cuadruplos[k+2] == 'T18' or Cuadruplos[k+2] == 'T19' or Cuadruplos[k+2] == 'T20' or Cuadruplos[k+2] == 'T21' or Cuadruplos[k+2] == 'T22' or Cuadruplos[k+2] == 'T23' or Cuadruplos[k+2] == 'T24' or Cuadruplos[k+2] == 'T25' or Cuadruplos[k+2] == 'T26' or Cuadruplos[k+2] == 'T27' or Cuadruplos[k+2] == 'T28' or Cuadruplos[k+2] == 'T29' or Cuadruplos[k+2] == 'T30' or Cuadruplos[k+2] == '$T11' or Cuadruplos[k+2] == '$T12' or Cuadruplos[k+2] == '$T13' or Cuadruplos[k+2] == '$T14' or Cuadruplos[k+2] == '$T15' or Cuadruplos[k+2] == '$T16' or Cuadruplos[k+2] == '$T17' or Cuadruplos[k+2] == '$T18' or Cuadruplos[k+2] == '$T19' or Cuadruplos[k+2] == '$T20' or Cuadruplos[k+2] == '$T21' or Cuadruplos[k+2] == '$T22' or Cuadruplos[k+2] == '$T23' or Cuadruplos[k+2] == '$T24' or Cuadruplos[k+2] == '$T25' or Cuadruplos[k+2] == '$T26' or Cuadruplos[k+2] == '$T27' or Cuadruplos[k+2] == '$T28' or Cuadruplos[k+2] == '$T29' or Cuadruplos[k+2] == '$T30'):
                if(Cuadruplos[k+2][0] != '$'):
                    Ind2=Avail.index(Cuadruplos[k+2])
                    b=AvailNum[Ind2]
                else:
                    Ind2=RAvail.index(Cuadruplos[k+2])
                    pos=RAvailVal[Ind2]
                    if(ArrayGiganteTipo[pos] == "Int"):
                        b=int(ArrayGigante[pos])
                    else:
                        b= float(ArrayGigante[pos])
                
            #Buscar en nombre tokens
            else:
                p=0
                j=0
                q=len(NombresTokens)
                while(p<q):
                    if(Cuadruplos[k+2] == NombresTokens[p]):
                        j=1
                        break
                    else:
                        p=p+1
                    
                    
                if(j == 1):
                    Ind2 = p
                    if(TipoTokens[Ind2] == "Int"):
                        b= int(ValoresTokens[Ind2])
                    elif(TipoTokens[Ind2] == "Flo"):
                        b= float(ValoresTokens[Ind2])

                else:
                    b=float(Cuadruplos[k+2])

            #Resultado
            if(Cuadruplos[k+3] == 'T1' or Cuadruplos[k+3] == '$T1' or Cuadruplos[k+3] == 'T2' or Cuadruplos[k+3] == '$T2' or Cuadruplos[k+3] == 'T3' or Cuadruplos[k+3] == '$T3' or Cuadruplos[k+3] == 'T4' or Cuadruplos[k+3] == '$T4' or Cuadruplos[k+3] == 'T5' or Cuadruplos[k+3] == '$T5' or Cuadruplos[k+3] == 'T6' or Cuadruplos[k+3] == '$T6' or Cuadruplos[k+3] == 'T7' or Cuadruplos[k+3] == '$T7' or Cuadruplos[k+3] == 'T8'  or Cuadruplos[k+3] == '$T8' or Cuadruplos[k+3] == 'T9' or Cuadruplos[k+3] == '$T9' or Cuadruplos[k+3] == 'T10' or Cuadruplos[k+3] == '$T10' or Cuadruplos[k+3] == 'T11' or Cuadruplos[k+3] == 'T12' or Cuadruplos[k+3] == 'T13' or Cuadruplos[k+3] == 'T14' or Cuadruplos[k+3] == 'T15' or Cuadruplos[k+3] == 'T16' or Cuadruplos[k+3] == 'T17' or Cuadruplos[k+3] == 'T18' or Cuadruplos[k+3] == 'T19' or Cuadruplos[k+3] == 'T20' or Cuadruplos[k+3] == 'T21' or Cuadruplos[k+3] == 'T22' or Cuadruplos[k+3] == 'T23' or Cuadruplos[k+3] == 'T24' or Cuadruplos[k+3] == 'T25' or Cuadruplos[k+3] == 'T26' or Cuadruplos[k+3] == 'T27' or Cuadruplos[k+3] == 'T28' or Cuadruplos[k+3] == 'T29' or Cuadruplos[k+3] == 'T30' or Cuadruplos[k+3] == '$T11' or Cuadruplos[k+3] == '$T12' or Cuadruplos[k+3] == '$T13' or Cuadruplos[k+3] == '$T14' or Cuadruplos[k+3] == '$T15' or Cuadruplos[k+3] == '$T16' or Cuadruplos[k+3] == '$T17' or Cuadruplos[k+3] == '$T18' or Cuadruplos[k+3] == '$T19' or Cuadruplos[k+3] == '$T20' or Cuadruplos[k+3] == '$T21' or Cuadruplos[k+3] == '$T22' or Cuadruplos[k+3] == '$T23' or Cuadruplos[k+3] == '$T24' or Cuadruplos[k+3] == '$T25' or Cuadruplos[k+3] == '$T26' or Cuadruplos[k+3] == '$T27' or Cuadruplos[k+3] == '$T28' or Cuadruplos[k+3] == '$T29' or Cuadruplos[k+3] == '$T30'):
                if(Cuadruplos[k+3][0] != '$'):
                    Ind3=Avail.index(Cuadruplos[k+3])
                    c = a + b
                    AvailNum[Ind3]=c
                    
                    
                elif(Cuadruplos[k+3][0] == '$' and Cheat == 1):
                    Pos=RAvail.index(Cuadruplos[k+3])
                    c = int(a + b)
                    RAvailVal[Pos]=c
                    Cheat=0
                else:
                    Pos=RAvail.index(Cuadruplos[k+3])
                    c = a + b
                    if(ArrayGiganteTipo[Pos] == "Int"):
                        ArrayGigante[Pos]= int(c)
                    else:
                        ArrayGigante[Pos]= float(c)
            else:
                Ind3=NombresTokens.index(Cuadruplos[k+3]) # Muestra error si no encuentra la viariable.
                c= a + b
                ValoresTokens[Ind3] = c
            PC = PC + 1
            
        # Resta  -------------------------------------------
        elif(Cuadruplos[k] == '-'):
            PCPila.append(PC)
            #Operando A
            #Avail o direcciones valores
            if(Cuadruplos[k+1] == 'T1' or Cuadruplos[k+1] == '$T1' or Cuadruplos[k+1] == 'T2' or Cuadruplos[k+1] == '$T2' or Cuadruplos[k+1] == 'T3' or Cuadruplos[k+1] == '$T3' or Cuadruplos[k+1] == 'T4' or Cuadruplos[k+1] == '$T4' or Cuadruplos[k+1] == 'T5' or Cuadruplos[k+1] == '$T5' or Cuadruplos[k+1] == 'T6' or Cuadruplos[k+1] == '$T6' or Cuadruplos[k+1] == 'T7' or Cuadruplos[k+1] == '$T7' or Cuadruplos[k+1] == 'T8'  or Cuadruplos[k+1] == '$T8' or Cuadruplos[k+1] == 'T9' or Cuadruplos[k+1] == '$T9' or Cuadruplos[k+1] == 'T10' or Cuadruplos[k+1] == '$T10' or Cuadruplos[k+1] == 'T11' or Cuadruplos[k+1] == 'T12' or Cuadruplos[k+1] == 'T13' or Cuadruplos[k+1] == 'T14' or Cuadruplos[k+1] == 'T15' or Cuadruplos[k+1] == 'T16' or Cuadruplos[k+1] == 'T17' or Cuadruplos[k+1] == 'T18' or Cuadruplos[k+1] == 'T19' or Cuadruplos[k+1] == 'T20' or Cuadruplos[k+1] == 'T21' or Cuadruplos[k+1] == 'T22' or Cuadruplos[k+1] == 'T23' or Cuadruplos[k+1] == 'T24' or Cuadruplos[k+1] == 'T25' or Cuadruplos[k+1] == 'T26' or Cuadruplos[k+1] == 'T27' or Cuadruplos[k+1] == 'T28' or Cuadruplos[k+1] == 'T29' or Cuadruplos[k+1] == 'T30' or Cuadruplos[k+1] == '$T11' or Cuadruplos[k+1] == '$T12' or Cuadruplos[k+1] == '$T13' or Cuadruplos[k+1] == '$T14' or Cuadruplos[k+1] == '$T15' or Cuadruplos[k+1] == '$T16' or Cuadruplos[k+1] == '$T17' or Cuadruplos[k+1] == '$T18' or Cuadruplos[k+1] == '$T19' or Cuadruplos[k+1] == '$T20' or Cuadruplos[k+1] == '$T21' or Cuadruplos[k+1] == '$T22' or Cuadruplos[k+1] == '$T23' or Cuadruplos[k+1] == '$T24' or Cuadruplos[k+1] == '$T25' or Cuadruplos[k+1] == '$T26' or Cuadruplos[k+1] == '$T27' or Cuadruplos[k+1] == '$T28' or Cuadruplos[k+1] == '$T29' or Cuadruplos[k+1] == '$T30'):
                if(Cuadruplos[k+1][0] != '$'):
                    Ind1=Avail.index(Cuadruplos[k+1])
                    a=AvailNum[Ind1]
                    
                else:
                    Ind1=RAvail.index(Cuadruplos[k+1])
                    pos=RAvailVal[Ind1]
                    if(ArrayGiganteTipo[pos] == "Int"):
                        a=int(ArrayGigante[pos])
                    else:
                        a= float(ArrayGigante[pos])
            #Buscar en nombre tokens
            else:
                p=0
                j=0
                q=len(NombresTokens)
                while(p<q):
                    if(Cuadruplos[k+1] == NombresTokens[p]):
                        j=1
                        break
                    else:
                        p=p+1
                        
                if(j == 1):
                    Ind1 = p
                    if(TipoTokens[Ind1] == "Int"):
                        a= int(ValoresTokens[Ind1])
                    elif(TipoTokens[Ind1] == "Flo"):
                        a= float(ValoresTokens[Ind1])
                else:
                    a=float(Cuadruplos[k+1])

            #Operando B
            #Avail o direcciones valores
            if(Cuadruplos[k+2] == 'T1' or Cuadruplos[k+2] == '$T1' or Cuadruplos[k+2] == 'T2' or Cuadruplos[k+2] == '$T2' or Cuadruplos[k+2] == 'T3' or Cuadruplos[k+2] == '$T3' or Cuadruplos[k+2] == 'T4' or Cuadruplos[k+2] == '$T4' or Cuadruplos[k+2] == 'T5' or Cuadruplos[k+2] == '$T5' or Cuadruplos[k+2] == 'T6' or Cuadruplos[k+2] == '$T6' or Cuadruplos[k+2] == 'T7' or Cuadruplos[k+2] == '$T7' or Cuadruplos[k+2] == 'T8'  or Cuadruplos[k+2] == '$T8' or Cuadruplos[k+2] == 'T9' or Cuadruplos[k+2] == '$T9' or Cuadruplos[k+2] == 'T10' or Cuadruplos[k+2] == '$T10' or Cuadruplos[k+2] == 'T11' or Cuadruplos[k+2] == 'T12' or Cuadruplos[k+2] == 'T13' or Cuadruplos[k+2] == 'T14' or Cuadruplos[k+2] == 'T15' or Cuadruplos[k+2] == 'T16' or Cuadruplos[k+2] == 'T17' or Cuadruplos[k+2] == 'T18' or Cuadruplos[k+2] == 'T19' or Cuadruplos[k+2] == 'T20' or Cuadruplos[k+2] == 'T21' or Cuadruplos[k+2] == 'T22' or Cuadruplos[k+2] == 'T23' or Cuadruplos[k+2] == 'T24' or Cuadruplos[k+2] == 'T25' or Cuadruplos[k+2] == 'T26' or Cuadruplos[k+2] == 'T27' or Cuadruplos[k+2] == 'T28' or Cuadruplos[k+2] == 'T29' or Cuadruplos[k+2] == 'T30' or Cuadruplos[k+2] == '$T11' or Cuadruplos[k+2] == '$T12' or Cuadruplos[k+2] == '$T13' or Cuadruplos[k+2] == '$T14' or Cuadruplos[k+2] == '$T15' or Cuadruplos[k+2] == '$T16' or Cuadruplos[k+2] == '$T17' or Cuadruplos[k+2] == '$T18' or Cuadruplos[k+2] == '$T19' or Cuadruplos[k+2] == '$T20' or Cuadruplos[k+2] == '$T21' or Cuadruplos[k+2] == '$T22' or Cuadruplos[k+2] == '$T23' or Cuadruplos[k+2] == '$T24' or Cuadruplos[k+2] == '$T25' or Cuadruplos[k+2] == '$T26' or Cuadruplos[k+2] == '$T27' or Cuadruplos[k+2] == '$T28' or Cuadruplos[k+2] == '$T29' or Cuadruplos[k+2] == '$T30'):
                if(Cuadruplos[k+2][0] != '$'):
                    Ind2=Avail.index(Cuadruplos[k+2])
                    b=AvailNum[Ind2]
                else:
                    Ind2=RAvail.index(Cuadruplos[k+2])
                    pos=RAvailVal[Ind2]
                    if(ArrayGiganteTipo[pos] == "Int"):
                        b=int(ArrayGigante[pos])
                    else:
                        b= float(ArrayGigante[pos])
               
            #Buscar en nombre tokens
            else:
                p=0
                j=0
                q=len(NombresTokens)
                while(p<q):
                    if(Cuadruplos[k+2] == NombresTokens[p]):
                        j=1
                        break
                    else:
                        p=p+1
                    
                    
                if(j == 1):
                    Ind2 = p
                    if(TipoTokens[Ind2] == "Int"):
                        b= int(ValoresTokens[Ind2])
                    elif(TipoTokens[Ind2] == "Flo"):
                        b= float(ValoresTokens[Ind2])

                else:
                    b=float(Cuadruplos[k+2])

            #Resultado
            if(Cuadruplos[k+3] == 'T1' or Cuadruplos[k+3] == '$T1' or Cuadruplos[k+3] == 'T2' or Cuadruplos[k+3] == '$T2' or Cuadruplos[k+3] == 'T3' or Cuadruplos[k+3] == '$T3' or Cuadruplos[k+3] == 'T4' or Cuadruplos[k+3] == '$T4' or Cuadruplos[k+3] == 'T5' or Cuadruplos[k+3] == '$T5' or Cuadruplos[k+3] == 'T6' or Cuadruplos[k+3] == '$T6' or Cuadruplos[k+3] == 'T7' or Cuadruplos[k+3] == '$T7' or Cuadruplos[k+3] == 'T8'  or Cuadruplos[k+3] == '$T8' or Cuadruplos[k+3] == 'T9' or Cuadruplos[k+3] == '$T9' or Cuadruplos[k+3] == 'T10' or Cuadruplos[k+3] == '$T10' or Cuadruplos[k+3] == 'T11' or Cuadruplos[k+3] == 'T12' or Cuadruplos[k+3] == 'T13' or Cuadruplos[k+3] == 'T14' or Cuadruplos[k+3] == 'T15' or Cuadruplos[k+3] == 'T16' or Cuadruplos[k+3] == 'T17' or Cuadruplos[k+3] == 'T18' or Cuadruplos[k+3] == 'T19' or Cuadruplos[k+3] == 'T20' or Cuadruplos[k+3] == 'T21' or Cuadruplos[k+3] == 'T22' or Cuadruplos[k+3] == 'T23' or Cuadruplos[k+3] == 'T24' or Cuadruplos[k+3] == 'T25' or Cuadruplos[k+3] == 'T26' or Cuadruplos[k+3] == 'T27' or Cuadruplos[k+3] == 'T28' or Cuadruplos[k+3] == 'T29' or Cuadruplos[k+3] == 'T30' or Cuadruplos[k+3] == '$T11' or Cuadruplos[k+3] == '$T12' or Cuadruplos[k+3] == '$T13' or Cuadruplos[k+3] == '$T14' or Cuadruplos[k+3] == '$T15' or Cuadruplos[k+3] == '$T16' or Cuadruplos[k+3] == '$T17' or Cuadruplos[k+3] == '$T18' or Cuadruplos[k+3] == '$T19' or Cuadruplos[k+3] == '$T20' or Cuadruplos[k+3] == '$T21' or Cuadruplos[k+3] == '$T22' or Cuadruplos[k+3] == '$T23' or Cuadruplos[k+3] == '$T24' or Cuadruplos[k+3] == '$T25' or Cuadruplos[k+3] == '$T26' or Cuadruplos[k+3] == '$T27' or Cuadruplos[k+3] == '$T28' or Cuadruplos[k+3] == '$T29' or Cuadruplos[k+3] == '$T30'):
                if(Cuadruplos[k+3][0] != '$'):
                    Ind3=Avail.index(Cuadruplos[k+3])
                    c = b - a
                    AvailNum[Ind3]=c
                    
                else:
                    Pos=RAvail.index(Cuadruplos[k+3])
                    c = b - a
                    if(ArrayGiganteTipo[Pos] == "Int"):
                        ArrayGigante[Pos]= int(c)
                    else:
                        ArrayGigante[Pos]= float(c)
            else:
                Ind3=NombresTokens.index(Cuadruplos[k+3]) # Muestra error si no encuentra la viariable.
                c= b - a
                ValoresTokens[Ind3] = c
            PC = PC + 1

        #Multiplicacion -------------------------------------------
        elif(Cuadruplos[k] == '*'):
            PCPila.append(PC)
            #Operando A
            #Avail o direcciones valores
            if(Cuadruplos[k+1] == 'T1' or Cuadruplos[k+1] == '$T1' or Cuadruplos[k+1] == 'T2' or Cuadruplos[k+1] == '$T2' or Cuadruplos[k+1] == 'T3' or Cuadruplos[k+1] == '$T3' or Cuadruplos[k+1] == 'T4' or Cuadruplos[k+1] == '$T4' or Cuadruplos[k+1] == 'T5' or Cuadruplos[k+1] == '$T5' or Cuadruplos[k+1] == 'T6' or Cuadruplos[k+1] == '$T6' or Cuadruplos[k+1] == 'T7' or Cuadruplos[k+1] == '$T7' or Cuadruplos[k+1] == 'T8'  or Cuadruplos[k+1] == '$T8' or Cuadruplos[k+1] == 'T9' or Cuadruplos[k+1] == '$T9' or Cuadruplos[k+1] == 'T10' or Cuadruplos[k+1] == '$T10' or Cuadruplos[k+1] == 'T11' or Cuadruplos[k+1] == 'T12' or Cuadruplos[k+1] == 'T13' or Cuadruplos[k+1] == 'T14' or Cuadruplos[k+1] == 'T15' or Cuadruplos[k+1] == 'T16' or Cuadruplos[k+1] == 'T17' or Cuadruplos[k+1] == 'T18' or Cuadruplos[k+1] == 'T19' or Cuadruplos[k+1] == 'T20' or Cuadruplos[k+1] == 'T21' or Cuadruplos[k+1] == 'T22' or Cuadruplos[k+1] == 'T23' or Cuadruplos[k+1] == 'T24' or Cuadruplos[k+1] == 'T25' or Cuadruplos[k+1] == 'T26' or Cuadruplos[k+1] == 'T27' or Cuadruplos[k+1] == 'T28' or Cuadruplos[k+1] == 'T29' or Cuadruplos[k+1] == 'T30' or Cuadruplos[k+1] == '$T11' or Cuadruplos[k+1] == '$T12' or Cuadruplos[k+1] == '$T13' or Cuadruplos[k+1] == '$T14' or Cuadruplos[k+1] == '$T15' or Cuadruplos[k+1] == '$T16' or Cuadruplos[k+1] == '$T17' or Cuadruplos[k+1] == '$T18' or Cuadruplos[k+1] == '$T19' or Cuadruplos[k+1] == '$T20' or Cuadruplos[k+1] == '$T21' or Cuadruplos[k+1] == '$T22' or Cuadruplos[k+1] == '$T23' or Cuadruplos[k+1] == '$T24' or Cuadruplos[k+1] == '$T25' or Cuadruplos[k+1] == '$T26' or Cuadruplos[k+1] == '$T27' or Cuadruplos[k+1] == '$T28' or Cuadruplos[k+1] == '$T29' or Cuadruplos[k+1] == '$T30'):
                if(Cuadruplos[k+1][0] != '$'):
                    Ind1=Avail.index(Cuadruplos[k+1])
                    a=AvailNum[Ind1]
                else:
                    Ind1=RAvail.index(Cuadruplos[k+1])
                    pos=RAvailVal[Ind1]
                    if(ArrayGiganteTipo[pos] == "Int"):
                        a=int(ArrayGigante[pos])
                    else:
                        a= float(ArrayGigante[pos])
                        
            #Buscar en nombre tokens
            else:
                p=0
                j=0
                q=len(NombresTokens)
                while(p<q):
                    if(Cuadruplos[k+1] == NombresTokens[p]):
                        j=1
                        break
                    else:
                        p=p+1
                    
                if(j == 1):
                    Ind1 = p
                    if(TipoTokens[Ind1] == "Int"):
                        a= int(ValoresTokens[Ind1])
                    elif(TipoTokens[Ind1] == "Flo"):
                        a= float(ValoresTokens[Ind1])

                else:
                    a=float(Cuadruplos[k+1])

            #Operando B
            if(Cuadruplos[k+2] == 'T1' or Cuadruplos[k+2] == '$T1' or Cuadruplos[k+2] == 'T2' or Cuadruplos[k+2] == '$T2' or Cuadruplos[k+2] == 'T3' or Cuadruplos[k+2] == '$T3' or Cuadruplos[k+2] == 'T4' or Cuadruplos[k+2] == '$T4' or Cuadruplos[k+2] == 'T5' or Cuadruplos[k+2] == '$T5' or Cuadruplos[k+2] == 'T6' or Cuadruplos[k+2] == '$T6' or Cuadruplos[k+2] == 'T7' or Cuadruplos[k+2] == '$T7' or Cuadruplos[k+2] == 'T8'  or Cuadruplos[k+2] == '$T8' or Cuadruplos[k+2] == 'T9' or Cuadruplos[k+2] == '$T9' or Cuadruplos[k+2] == 'T10' or Cuadruplos[k+2] == '$T10' or Cuadruplos[k+2] == 'T11' or Cuadruplos[k+2] == 'T12' or Cuadruplos[k+2] == 'T13' or Cuadruplos[k+2] == 'T14' or Cuadruplos[k+2] == 'T15' or Cuadruplos[k+2] == 'T16' or Cuadruplos[k+2] == 'T17' or Cuadruplos[k+2] == 'T18' or Cuadruplos[k+2] == 'T19' or Cuadruplos[k+2] == 'T20' or Cuadruplos[k+2] == 'T21' or Cuadruplos[k+2] == 'T22' or Cuadruplos[k+2] == 'T23' or Cuadruplos[k+2] == 'T24' or Cuadruplos[k+2] == 'T25' or Cuadruplos[k+2] == 'T26' or Cuadruplos[k+2] == 'T27' or Cuadruplos[k+2] == 'T28' or Cuadruplos[k+2] == 'T29' or Cuadruplos[k+2] == 'T30' or Cuadruplos[k+2] == '$T11' or Cuadruplos[k+2] == '$T12' or Cuadruplos[k+2] == '$T13' or Cuadruplos[k+2] == '$T14' or Cuadruplos[k+2] == '$T15' or Cuadruplos[k+2] == '$T16' or Cuadruplos[k+2] == '$T17' or Cuadruplos[k+2] == '$T18' or Cuadruplos[k+2] == '$T19' or Cuadruplos[k+2] == '$T20' or Cuadruplos[k+2] == '$T21' or Cuadruplos[k+2] == '$T22' or Cuadruplos[k+2] == '$T23' or Cuadruplos[k+2] == '$T24' or Cuadruplos[k+2] == '$T25' or Cuadruplos[k+2] == '$T26' or Cuadruplos[k+2] == '$T27' or Cuadruplos[k+2] == '$T28' or Cuadruplos[k+2] == '$T29' or Cuadruplos[k+2] == '$T30'):
                if(Cuadruplos[k+2][0] != '$'):
                    Ind2=Avail.index(Cuadruplos[k+2])
                    b=AvailNum[Ind2]
                else:
                    Ind2=RAvail.index(Cuadruplos[k+2])
                    pos=RAvailVal[Ind2]
                    if(ArrayGiganteTipo[pos] == "Int"):
                        b=int(ArrayGigante[pos])
                    else:
                        b= float(ArrayGigante[pos])
                
            #Buscar en nombre tokens
            else:
                p=0
                j=0
                q=len(NombresTokens)
                while(p<q):
                    if(Cuadruplos[k+2] == NombresTokens[p]):
                        j=1
                        break
                    else:
                        p=p+1
                        
                if(j == 1):
                    Ind2 = p
                    if(TipoTokens[Ind2] == "Int"):
                        b= int(ValoresTokens[Ind2])
                    elif(TipoTokens[Ind2] == "Flo"):
                        b= float(ValoresTokens[Ind2])

                else:
                    b=float(Cuadruplos[k+2])

            #Resultado
            if(Cuadruplos[k+3] == 'T1' or Cuadruplos[k+3] == '$T1' or Cuadruplos[k+3] == 'T2' or Cuadruplos[k+3] == '$T2' or Cuadruplos[k+3] == 'T3' or Cuadruplos[k+3] == '$T3' or Cuadruplos[k+3] == 'T4' or Cuadruplos[k+3] == '$T4' or Cuadruplos[k+3] == 'T5' or Cuadruplos[k+3] == '$T5' or Cuadruplos[k+3] == 'T6' or Cuadruplos[k+3] == '$T6' or Cuadruplos[k+3] == 'T7' or Cuadruplos[k+3] == '$T7' or Cuadruplos[k+3] == 'T8'  or Cuadruplos[k+3] == '$T8' or Cuadruplos[k+3] == 'T9' or Cuadruplos[k+3] == '$T9' or Cuadruplos[k+3] == 'T10' or Cuadruplos[k+3] == '$T10' or Cuadruplos[k+3] == 'T11' or Cuadruplos[k+3] == 'T12' or Cuadruplos[k+3] == 'T13' or Cuadruplos[k+3] == 'T14' or Cuadruplos[k+3] == 'T15' or Cuadruplos[k+3] == 'T16' or Cuadruplos[k+3] == 'T17' or Cuadruplos[k+3] == 'T18' or Cuadruplos[k+3] == 'T19' or Cuadruplos[k+3] == 'T20' or Cuadruplos[k+3] == 'T21' or Cuadruplos[k+3] == 'T22' or Cuadruplos[k+3] == 'T23' or Cuadruplos[k+3] == 'T24' or Cuadruplos[k+3] == 'T25' or Cuadruplos[k+3] == 'T26' or Cuadruplos[k+3] == 'T27' or Cuadruplos[k+3] == 'T28' or Cuadruplos[k+3] == 'T29' or Cuadruplos[k+3] == 'T30' or Cuadruplos[k+3] == '$T11' or Cuadruplos[k+3] == '$T12' or Cuadruplos[k+3] == '$T13' or Cuadruplos[k+3] == '$T14' or Cuadruplos[k+3] == '$T15' or Cuadruplos[k+3] == '$T16' or Cuadruplos[k+3] == '$T17' or Cuadruplos[k+3] == '$T18' or Cuadruplos[k+3] == '$T19' or Cuadruplos[k+3] == '$T20' or Cuadruplos[k+3] == '$T21' or Cuadruplos[k+3] == '$T22' or Cuadruplos[k+3] == '$T23' or Cuadruplos[k+3] == '$T24' or Cuadruplos[k+3] == '$T25' or Cuadruplos[k+3] == '$T26' or Cuadruplos[k+3] == '$T27' or Cuadruplos[k+3] == '$T28' or Cuadruplos[k+3] == '$T29' or Cuadruplos[k+3] == '$T30'):
                if(Cuadruplos[k+3][0] != '$'):
                    Ind3=Avail.index(Cuadruplos[k+3])
                    c = b * a
                    AvailNum[Ind3]=c
                    
                else:
                    Pos=RAvail.index(Cuadruplos[k+3])
                    c = b * a
                    if(ArrayGiganteTipo[Pos] == "Int"):
                        ArrayGigante[Pos]= int(c)
                    else:
                        ArrayGigante[Pos]= float(c)
            else:
                Ind3=NombresTokens.index(Cuadruplos[k+3]) # Muestra error si no encuentra la viariable.
                c= a * b
                ValoresTokens[Ind3] = c
            PC = PC + 1
            
        # Division  -------------------------------------------
        elif(Cuadruplos[k] == '/'):
            PCPila.append(PC)
            #Operando A
            #Avail o direcciones valores
            if(Cuadruplos[k+1] == 'T1' or Cuadruplos[k+1] == '$T1' or Cuadruplos[k+1] == 'T2' or Cuadruplos[k+1] == '$T2' or Cuadruplos[k+1] == 'T3' or Cuadruplos[k+1] == '$T3' or Cuadruplos[k+1] == 'T4' or Cuadruplos[k+1] == '$T4' or Cuadruplos[k+1] == 'T5' or Cuadruplos[k+1] == '$T5' or Cuadruplos[k+1] == 'T6' or Cuadruplos[k+1] == '$T6' or Cuadruplos[k+1] == 'T7' or Cuadruplos[k+1] == '$T7' or Cuadruplos[k+1] == 'T8'  or Cuadruplos[k+1] == '$T8' or Cuadruplos[k+1] == 'T9' or Cuadruplos[k+1] == '$T9' or Cuadruplos[k+1] == 'T10' or Cuadruplos[k+1] == '$T10' or Cuadruplos[k+1] == 'T11' or Cuadruplos[k+1] == 'T12' or Cuadruplos[k+1] == 'T13' or Cuadruplos[k+1] == 'T14' or Cuadruplos[k+1] == 'T15' or Cuadruplos[k+1] == 'T16' or Cuadruplos[k+1] == 'T17' or Cuadruplos[k+1] == 'T18' or Cuadruplos[k+1] == 'T19' or Cuadruplos[k+1] == 'T20' or Cuadruplos[k+1] == 'T21' or Cuadruplos[k+1] == 'T22' or Cuadruplos[k+1] == 'T23' or Cuadruplos[k+1] == 'T24' or Cuadruplos[k+1] == 'T25' or Cuadruplos[k+1] == 'T26' or Cuadruplos[k+1] == 'T27' or Cuadruplos[k+1] == 'T28' or Cuadruplos[k+1] == 'T29' or Cuadruplos[k+1] == 'T30' or Cuadruplos[k+1] == '$T11' or Cuadruplos[k+1] == '$T12' or Cuadruplos[k+1] == '$T13' or Cuadruplos[k+1] == '$T14' or Cuadruplos[k+1] == '$T15' or Cuadruplos[k+1] == '$T16' or Cuadruplos[k+1] == '$T17' or Cuadruplos[k+1] == '$T18' or Cuadruplos[k+1] == '$T19' or Cuadruplos[k+1] == '$T20' or Cuadruplos[k+1] == '$T21' or Cuadruplos[k+1] == '$T22' or Cuadruplos[k+1] == '$T23' or Cuadruplos[k+1] == '$T24' or Cuadruplos[k+1] == '$T25' or Cuadruplos[k+1] == '$T26' or Cuadruplos[k+1] == '$T27' or Cuadruplos[k+1] == '$T28' or Cuadruplos[k+1] == '$T29' or Cuadruplos[k+1] == '$T30'):
                if(Cuadruplos[k+1][0] != '$'):
                    Ind1=Avail.index(Cuadruplos[k+1])
                    a=AvailNum[Ind1]
                else:
                    Ind1=RAvail.index(Cuadruplos[k+1])
                    pos=RAvailVal[Ind1]
                    if(ArrayGiganteTipo[pos] == "Int"):
                        a=int(ArrayGigante[pos])
                    else:
                        a= float(ArrayGigante[pos])

            #Buscar en nombre tokens
            else:
                p=0
                j=0
                q=len(NombresTokens)
                while(p<q):
                    if(Cuadruplos[k+1] == NombresTokens[p]):
                        j=1
                        break
                    else:
                        p=p+1
                        
                if(j == 1):
                    Ind1 = p
                    if(TipoTokens[Ind1] == "Int"):
                        a= int(ValoresTokens[Ind1])
                    elif(TipoTokens[Ind1] == "Flo"):
                        a= float(ValoresTokens[Ind1])
                else:
                    a=float(Cuadruplos[k+1])

            #Operando B
            #Avail o direcciones valores
            if(Cuadruplos[k+2] == 'T1' or Cuadruplos[k+2] == '$T1' or Cuadruplos[k+2] == 'T2' or Cuadruplos[k+2] == '$T2' or Cuadruplos[k+2] == 'T3' or Cuadruplos[k+2] == '$T3' or Cuadruplos[k+2] == 'T4' or Cuadruplos[k+2] == '$T4' or Cuadruplos[k+2] == 'T5' or Cuadruplos[k+2] == '$T5' or Cuadruplos[k+2] == 'T6' or Cuadruplos[k+2] == '$T6' or Cuadruplos[k+2] == 'T7' or Cuadruplos[k+2] == '$T7' or Cuadruplos[k+2] == 'T8'  or Cuadruplos[k+2] == '$T8' or Cuadruplos[k+2] == 'T9' or Cuadruplos[k+2] == '$T9' or Cuadruplos[k+2] == 'T10' or Cuadruplos[k+2] == '$T10' or Cuadruplos[k+2] == 'T11' or Cuadruplos[k+2] == 'T12' or Cuadruplos[k+2] == 'T13' or Cuadruplos[k+2] == 'T14' or Cuadruplos[k+2] == 'T15' or Cuadruplos[k+2] == 'T16' or Cuadruplos[k+2] == 'T17' or Cuadruplos[k+2] == 'T18' or Cuadruplos[k+2] == 'T19' or Cuadruplos[k+2] == 'T20' or Cuadruplos[k+2] == 'T21' or Cuadruplos[k+2] == 'T22' or Cuadruplos[k+2] == 'T23' or Cuadruplos[k+2] == 'T24' or Cuadruplos[k+2] == 'T25' or Cuadruplos[k+2] == 'T26' or Cuadruplos[k+2] == 'T27' or Cuadruplos[k+2] == 'T28' or Cuadruplos[k+2] == 'T29' or Cuadruplos[k+2] == 'T30' or Cuadruplos[k+2] == '$T11' or Cuadruplos[k+2] == '$T12' or Cuadruplos[k+2] == '$T13' or Cuadruplos[k+2] == '$T14' or Cuadruplos[k+2] == '$T15' or Cuadruplos[k+2] == '$T16' or Cuadruplos[k+2] == '$T17' or Cuadruplos[k+2] == '$T18' or Cuadruplos[k+2] == '$T19' or Cuadruplos[k+2] == '$T20' or Cuadruplos[k+2] == '$T21' or Cuadruplos[k+2] == '$T22' or Cuadruplos[k+2] == '$T23' or Cuadruplos[k+2] == '$T24' or Cuadruplos[k+2] == '$T25' or Cuadruplos[k+2] == '$T26' or Cuadruplos[k+2] == '$T27' or Cuadruplos[k+2] == '$T28' or Cuadruplos[k+2] == '$T29' or Cuadruplos[k+2] == '$T30'):
                if(Cuadruplos[k+2][0] != '$'):
                    Ind2=Avail.index(Cuadruplos[k+2])
                    b=AvailNum[Ind2]

                else:
                    Ind2=RAvail.index(Cuadruplos[k+2])
                    pos=RAvailVal[Ind2]
                    if(ArrayGiganteTipo[pos] == "Int"):
                        b=int(ArrayGigante[pos])
                    else:
                        b= float(ArrayGigante[pos])
                        
            #Buscar en nombre tokens
            else:
                p=0
                j=0
                q=len(NombresTokens)
                while(p<q):
                    if(Cuadruplos[k+2] == NombresTokens[p]):
                        j=1
                        break
                    else:
                        p=p+1
                    
                    
                if(j == 1):
                    Ind2 = p
                    if(TipoTokens[Ind2] == "Int"):
                        b= int(ValoresTokens[Ind2])
                    elif(TipoTokens[Ind2] == "Flo"):
                        b= float(ValoresTokens[Ind2])

                else:
                    b=float(Cuadruplos[k+2])

            #Resultado
            if(Cuadruplos[k+3] == 'T1' or Cuadruplos[k+3] == '$T1' or Cuadruplos[k+3] == 'T2' or Cuadruplos[k+3] == '$T2' or Cuadruplos[k+3] == 'T3' or Cuadruplos[k+3] == '$T3' or Cuadruplos[k+3] == 'T4' or Cuadruplos[k+3] == '$T4' or Cuadruplos[k+3] == 'T5' or Cuadruplos[k+3] == '$T5' or Cuadruplos[k+3] == 'T6' or Cuadruplos[k+3] == '$T6' or Cuadruplos[k+3] == 'T7' or Cuadruplos[k+3] == '$T7' or Cuadruplos[k+3] == 'T8'  or Cuadruplos[k+3] == '$T8' or Cuadruplos[k+3] == 'T9' or Cuadruplos[k+3] == '$T9' or Cuadruplos[k+3] == 'T10' or Cuadruplos[k+3] == '$T10' or Cuadruplos[k+3] == 'T11' or Cuadruplos[k+3] == 'T12' or Cuadruplos[k+3] == 'T13' or Cuadruplos[k+3] == 'T14' or Cuadruplos[k+3] == 'T15' or Cuadruplos[k+3] == 'T16' or Cuadruplos[k+3] == 'T17' or Cuadruplos[k+3] == 'T18' or Cuadruplos[k+3] == 'T19' or Cuadruplos[k+3] == 'T20' or Cuadruplos[k+3] == 'T21' or Cuadruplos[k+3] == 'T22' or Cuadruplos[k+3] == 'T23' or Cuadruplos[k+3] == 'T24' or Cuadruplos[k+3] == 'T25' or Cuadruplos[k+3] == 'T26' or Cuadruplos[k+3] == 'T27' or Cuadruplos[k+3] == 'T28' or Cuadruplos[k+3] == 'T29' or Cuadruplos[k+3] == 'T30' or Cuadruplos[k+3] == '$T11' or Cuadruplos[k+3] == '$T12' or Cuadruplos[k+3] == '$T13' or Cuadruplos[k+3] == '$T14' or Cuadruplos[k+3] == '$T15' or Cuadruplos[k+3] == '$T16' or Cuadruplos[k+3] == '$T17' or Cuadruplos[k+3] == '$T18' or Cuadruplos[k+3] == '$T19' or Cuadruplos[k+3] == '$T20' or Cuadruplos[k+3] == '$T21' or Cuadruplos[k+3] == '$T22' or Cuadruplos[k+3] == '$T23' or Cuadruplos[k+3] == '$T24' or Cuadruplos[k+3] == '$T25' or Cuadruplos[k+3] == '$T26' or Cuadruplos[k+3] == '$T27' or Cuadruplos[k+3] == '$T28' or Cuadruplos[k+3] == '$T29' or Cuadruplos[k+3] == '$T30'):
                if(Cuadruplos[k+3][0] != '$'):
                    Ind3=Avail.index(Cuadruplos[k+3])
                    c = b/a
                    AvailNum[Ind3]=c

                if(Cuadruplos[k+3][0] != '$'):
                    Ind3=Avail.index(Cuadruplos[k+3])
                    c = b / a
                    AvailNum[Ind3]=c
                    
                else:
                    Pos=RAvail.index(Cuadruplos[k+3])
                    c = b / a
                    if(ArrayGiganteTipo[Pos] == "Int"):
                        ArrayGigante[Pos]= int(c)
                    else:
                        ArrayGigante[Pos]= float(c)
            else:
                Ind3=NombresTokens.index(Cuadruplos[k+3]) # Muestra error si no encuentra la viariable.
                c= b/a
                ValoresTokens[Ind3] = c
            PC = PC + 1
      
        #Comparacion igualdad -------------------------------------------
        elif(Cuadruplos[k] == '='):
            PCPila.append(PC)
            #Operando A
            if(Cuadruplos[k+1] == 'T1' or Cuadruplos[k+1] == 'T2' or Cuadruplos[k+1] == 'T3' or Cuadruplos[k+1] == 'T4' or Cuadruplos[k+1] == 'T5' or Cuadruplos[k+1] == 'T6' or Cuadruplos[k+1] == 'T7' or Cuadruplos[k+1] == 'T8' or Cuadruplos[k+1] == 'T9' or Cuadruplos[k+1] == 'T10' or Cuadruplos[k+1] == 'T11' or Cuadruplos[k+1] == 'T12' or Cuadruplos[k+1] == 'T13' or Cuadruplos[k+1] == 'T14' or Cuadruplos[k+1] == 'T15' or Cuadruplos[k+1] == 'T16' or Cuadruplos[k+1] == 'T17' or Cuadruplos[k+1] == 'T18' or Cuadruplos[k+1] == 'T19' or Cuadruplos[k+1] == 'T20'or Cuadruplos[k+1] == 'T21' or Cuadruplos[k+1] == 'T22' or Cuadruplos[k+1] == 'T23' or Cuadruplos[k+1] == 'T24' or Cuadruplos[k+1] == 'T25' or Cuadruplos[k+1] == 'T26' or Cuadruplos[k+1] == 'T27' or Cuadruplos[k+1] == 'T28' or Cuadruplos[k+1] == 'T29' or Cuadruplos[k+1] == 'T30'):
                Ind1=Avail.index(Cuadruplos[k+1])
                a=AvailNum[Ind1]
            else:
                Ind1=NombresTokens.index(Cuadruplos[k+1]) # Muestra error si no encuentra la viariable.
                if(TipoTokens[Ind1] == "Int"):
                    a= int(ValoresTokens[Ind1])
                elif(TipoTokens[Ind1] == "Flo"):
                    a= float(ValoresTokens[Ind1])
            #Operando B
            if(Cuadruplos[k+2] == 'T1' or Cuadruplos[k+2] == 'T2' or Cuadruplos[k+2] == 'T3' or Cuadruplos[k+2] == 'T4' or Cuadruplos[k+2] == 'T5' or Cuadruplos[k+2] == 'T6' or Cuadruplos[k+2] == 'T7' or Cuadruplos[k+2] == 'T8' or Cuadruplos[k+2] == 'T9' or Cuadruplos[k+2] == 'T10' or Cuadruplos[k+2] == 'T11' or Cuadruplos[k+2] == 'T12' or Cuadruplos[k+2] == 'T13' or Cuadruplos[k+2] == 'T14' or Cuadruplos[k+2] == 'T15' or Cuadruplos[k+2] == 'T16' or Cuadruplos[k+2] == 'T17' or Cuadruplos[k+2] == 'T18' or Cuadruplos[k+2] == 'T19' or Cuadruplos[k+2] == 'T20'or Cuadruplos[k+2] == 'T21' or Cuadruplos[k+2] == 'T22' or Cuadruplos[k+2] == 'T23' or Cuadruplos[k+2] == 'T24' or Cuadruplos[k+2] == 'T25' or Cuadruplos[k+2] == 'T26' or Cuadruplos[k+2] == 'T27' or Cuadruplos[k+2] == 'T28' or Cuadruplos[k+2] == 'T29' or Cuadruplos[k+2] == 'T30'):
                Ind2=Avail.index(Cuadruplos[k+2])
                b=AvailNum[Ind2]
            else:
                Ind2=NombresTokens.index(Cuadruplos[k+2]) # Muestra error si no encuentra la viariable.
                if(TipoTokens[Ind2] == "Int"):
                    b= int(ValoresTokens[Ind2])
                elif(TipoTokens[Ind2] == "Flo"):
                    b= float(ValoresTokens[Ind2]) 
            #Resultado
            if(Cuadruplos[k+3] == 'T1' or Cuadruplos[k+3] == 'T2' or Cuadruplos[k+3] == 'T3' or Cuadruplos[k+3] == 'T4' or Cuadruplos[k+3] == 'T5' or Cuadruplos[k+3] == 'T6' or Cuadruplos[k+3] == 'T7' or Cuadruplos[k+3] == 'T8' or Cuadruplos[k+3] == 'T9' or Cuadruplos[k+3] == 'T10' or Cuadruplos[k+3] == 'T11' or Cuadruplos[k+3] == 'T12' or Cuadruplos[k+3] == 'T13' or Cuadruplos[k+3] == 'T14' or Cuadruplos[k+3] == 'T15' or Cuadruplos[k+3] == 'T16' or Cuadruplos[k+3] == 'T17' or Cuadruplos[k+3] == 'T18' or Cuadruplos[k+3] == 'T19' or Cuadruplos[k+3] == 'T20'or Cuadruplos[k+3] == 'T21' or Cuadruplos[k+3] == 'T22' or Cuadruplos[k+3] == 'T23' or Cuadruplos[k+3] == 'T24' or Cuadruplos[k+3] == 'T25' or Cuadruplos[k+3] == 'T26' or Cuadruplos[k+3] == 'T27' or Cuadruplos[k+3] == 'T28' or Cuadruplos[k+3] == 'T29' or Cuadruplos[k+3] == 'T30'):
                Ind3=Avail.index(Cuadruplos[k+3])
                if(a == b):
                    AvailBool[Ind3] = True
                    PC = PC + 2
                else:
                    AvailBool[Ind3] = False
                    PC = PC + 1
        #Comparacion Menor -------------------------------------------
        elif(Cuadruplos[k] == '<'):
            PCPila.append(PC)
            #Operando A
            if(Cuadruplos[k+1] == 'T1' or Cuadruplos[k+1] == 'T2' or Cuadruplos[k+1] == 'T3' or Cuadruplos[k+1] == 'T4' or Cuadruplos[k+1] == 'T5' or Cuadruplos[k+1] == 'T6' or Cuadruplos[k+1] == 'T7' or Cuadruplos[k+1] == 'T8' or Cuadruplos[k+1] == 'T9' or Cuadruplos[k+1] == 'T10' or Cuadruplos[k+1] == 'T11' or Cuadruplos[k+1] == 'T12' or Cuadruplos[k+1] == 'T13' or Cuadruplos[k+1] == 'T14' or Cuadruplos[k+1] == 'T15' or Cuadruplos[k+1] == 'T16' or Cuadruplos[k+1] == 'T17' or Cuadruplos[k+1] == 'T18' or Cuadruplos[k+1] == 'T19' or Cuadruplos[k+1] == 'T20'or Cuadruplos[k+1] == 'T21' or Cuadruplos[k+1] == 'T22' or Cuadruplos[k+1] == 'T23' or Cuadruplos[k+1] == 'T24' or Cuadruplos[k+1] == 'T25' or Cuadruplos[k+1] == 'T26' or Cuadruplos[k+1] == 'T27' or Cuadruplos[k+1] == 'T28' or Cuadruplos[k+1] == 'T29' or Cuadruplos[k+1] == 'T30'):
                Ind1=Avail.index(Cuadruplos[k+1])
                a=AvailNum[Ind1]
            else:
                Ind1=NombresTokens.index(Cuadruplos[k+1]) # Muestra error si no encuentra la viariable.
                if(TipoTokens[Ind1] == "Int"):
                    a= int(ValoresTokens[Ind1])
                elif(TipoTokens[Ind1] == "Flo"):
                    a= float(ValoresTokens[Ind1])
            #Operando B
            if(Cuadruplos[k+2] == 'T1' or Cuadruplos[k+2] == 'T2' or Cuadruplos[k+2] == 'T3' or Cuadruplos[k+2] == 'T4' or Cuadruplos[k+2] == 'T5' or Cuadruplos[k+2] == 'T6' or Cuadruplos[k+2] == 'T7' or Cuadruplos[k+2] == 'T8' or Cuadruplos[k+2] == 'T9' or Cuadruplos[k+2] == 'T10' or Cuadruplos[k+2] == 'T11' or Cuadruplos[k+2] == 'T12' or Cuadruplos[k+2] == 'T13' or Cuadruplos[k+2] == 'T14' or Cuadruplos[k+2] == 'T15' or Cuadruplos[k+2] == 'T16' or Cuadruplos[k+2] == 'T17' or Cuadruplos[k+2] == 'T18' or Cuadruplos[k+2] == 'T19' or Cuadruplos[k+2] == 'T20'or Cuadruplos[k+2] == 'T21' or Cuadruplos[k+2] == 'T22' or Cuadruplos[k+2] == 'T23' or Cuadruplos[k+2] == 'T24' or Cuadruplos[k+2] == 'T25' or Cuadruplos[k+2] == 'T26' or Cuadruplos[k+2] == 'T27' or Cuadruplos[k+2] == 'T28' or Cuadruplos[k+2] == 'T29' or Cuadruplos[k+2] == 'T30'):
                Ind2=Avail.index(Cuadruplos[k+2])
                b=AvailNum[Ind2]
            else:
                Ind2=NombresTokens.index(Cuadruplos[k+2]) # Muestra error si no encuentra la viariable.
                if(TipoTokens[Ind2] == "Int"):
                    b= int(ValoresTokens[Ind2])
                elif(TipoTokens[Ind2] == "Flo"):
                    b= float(ValoresTokens[Ind2]) 
            #Resultado
            if(Cuadruplos[k+3] == 'T1' or Cuadruplos[k+3] == 'T2' or Cuadruplos[k+3] == 'T3' or Cuadruplos[k+3] == 'T4' or Cuadruplos[k+3] == 'T5' or Cuadruplos[k+3] == 'T6' or Cuadruplos[k+3] == 'T7' or Cuadruplos[k+3] == 'T8' or Cuadruplos[k+3] == 'T9' or Cuadruplos[k+3] == 'T10' or Cuadruplos[k+3] == 'T11' or Cuadruplos[k+3] == 'T12' or Cuadruplos[k+3] == 'T13' or Cuadruplos[k+3] == 'T14' or Cuadruplos[k+3] == 'T15' or Cuadruplos[k+3] == 'T16' or Cuadruplos[k+3] == 'T17' or Cuadruplos[k+3] == 'T18' or Cuadruplos[k+3] == 'T19' or Cuadruplos[k+3] == 'T20'or Cuadruplos[k+3] == 'T21' or Cuadruplos[k+3] == 'T22' or Cuadruplos[k+3] == 'T23' or Cuadruplos[k+3] == 'T24' or Cuadruplos[k+3] == 'T25' or Cuadruplos[k+3] == 'T26' or Cuadruplos[k+3] == 'T27' or Cuadruplos[k+3] == 'T28' or Cuadruplos[k+3] == 'T29' or Cuadruplos[k+3] == 'T30'):
                Ind3=Avail.index(Cuadruplos[k+3])
                if(a < b):
                    AvailBool[Ind3] = True
                    PC = PC + 2
                else:
                    AvailBool[Ind3] = False
                    PC = PC + 1
        #Comparacion Mayor -------------------------------------------
        elif(Cuadruplos[k] == '>'):
            PCPila.append(PC)
            #Operando A
            if(Cuadruplos[k+1] == 'T1' or Cuadruplos[k+1] == 'T2' or Cuadruplos[k+1] == 'T3' or Cuadruplos[k+1] == 'T4' or Cuadruplos[k+1] == 'T5' or Cuadruplos[k+1] == 'T6' or Cuadruplos[k+1] == 'T7' or Cuadruplos[k+1] == 'T8' or Cuadruplos[k+1] == 'T9' or Cuadruplos[k+1] == 'T10' or Cuadruplos[k+1] == 'T11' or Cuadruplos[k+1] == 'T12' or Cuadruplos[k+1] == 'T13' or Cuadruplos[k+1] == 'T14' or Cuadruplos[k+1] == 'T15' or Cuadruplos[k+1] == 'T16' or Cuadruplos[k+1] == 'T17' or Cuadruplos[k+1] == 'T18' or Cuadruplos[k+1] == 'T19' or Cuadruplos[k+1] == 'T20'or Cuadruplos[k+1] == 'T21' or Cuadruplos[k+1] == 'T22' or Cuadruplos[k+1] == 'T23' or Cuadruplos[k+1] == 'T24' or Cuadruplos[k+1] == 'T25' or Cuadruplos[k+1] == 'T26' or Cuadruplos[k+1] == 'T27' or Cuadruplos[k+1] == 'T28' or Cuadruplos[k+1] == 'T29' or Cuadruplos[k+1] == 'T30'):
                Ind1=Avail.index(Cuadruplos[k+1])
                a=AvailNum[Ind1]
            else:
                Ind1=NombresTokens.index(Cuadruplos[k+1]) # Muestra error si no encuentra la viariable.
                if(TipoTokens[Ind1] == "Int"):
                    a= int(ValoresTokens[Ind1])
                elif(TipoTokens[Ind1] == "Flo"):
                    a= float(ValoresTokens[Ind1])
            #Operando B
            if(Cuadruplos[k+2] == 'T1' or Cuadruplos[k+2] == 'T2' or Cuadruplos[k+2] == 'T3' or Cuadruplos[k+2] == 'T4' or Cuadruplos[k+2] == 'T5' or Cuadruplos[k+2] == 'T6' or Cuadruplos[k+2] == 'T7' or Cuadruplos[k+2] == 'T8' or Cuadruplos[k+2] == 'T9' or Cuadruplos[k+2] == 'T10' or Cuadruplos[k+2] == 'T11' or Cuadruplos[k+2] == 'T12' or Cuadruplos[k+2] == 'T13' or Cuadruplos[k+2] == 'T14' or Cuadruplos[k+2] == 'T15' or Cuadruplos[k+2] == 'T16' or Cuadruplos[k+2] == 'T17' or Cuadruplos[k+2] == 'T18' or Cuadruplos[k+2] == 'T19' or Cuadruplos[k+2] == 'T20'or Cuadruplos[k+2] == 'T21' or Cuadruplos[k+2] == 'T22' or Cuadruplos[k+2] == 'T23' or Cuadruplos[k+2] == 'T24' or Cuadruplos[k+2] == 'T25' or Cuadruplos[k+2] == 'T26' or Cuadruplos[k+2] == 'T27' or Cuadruplos[k+2] == 'T28' or Cuadruplos[k+2] == 'T29' or Cuadruplos[k+2] == 'T30'):
                Ind2=Avail.index(Cuadruplos[k+2])
                b=AvailNum[Ind2]
            else:
                Ind2=NombresTokens.index(Cuadruplos[k+2]) # Muestra error si no encuentra la viariable.
                if(TipoTokens[Ind2] == "Int"):
                    b= int(ValoresTokens[Ind2])
                elif(TipoTokens[Ind2] == "Flo"):
                    b= float(ValoresTokens[Ind2]) 
            #Resultado
            if(Cuadruplos[k+3] == 'T1' or Cuadruplos[k+3] == 'T2' or Cuadruplos[k+3] == 'T3' or Cuadruplos[k+3] == 'T4' or Cuadruplos[k+3] == 'T5' or Cuadruplos[k+3] == 'T6' or Cuadruplos[k+3] == 'T7' or Cuadruplos[k+3] == 'T8' or Cuadruplos[k+3] == 'T9' or Cuadruplos[k+3] == 'T10' or Cuadruplos[k+3] == 'T11' or Cuadruplos[k+3] == 'T12' or Cuadruplos[k+3] == 'T13' or Cuadruplos[k+3] == 'T14' or Cuadruplos[k+3] == 'T15' or Cuadruplos[k+3] == 'T16' or Cuadruplos[k+3] == 'T17' or Cuadruplos[k+3] == 'T18' or Cuadruplos[k+3] == 'T19' or Cuadruplos[k+3] == 'T20'or Cuadruplos[k+3] == 'T21' or Cuadruplos[k+3] == 'T22' or Cuadruplos[k+3] == 'T23' or Cuadruplos[k+3] == 'T24' or Cuadruplos[k+3] == 'T25' or Cuadruplos[k+3] == 'T26' or Cuadruplos[k+3] == 'T27' or Cuadruplos[k+3] == 'T28' or Cuadruplos[k+3] == 'T29' or Cuadruplos[k+3] == 'T30'):
                Ind3=Avail.index(Cuadruplos[k+3])
                if(a > b):
                    AvailBool[Ind3] = True
                    PC = PC + 2
                else:
                    AvailBool[Ind3] = False
                    PC = PC + 1

        #Goto procedimiento de los metodos -------------------------------------------
        elif(Cuadruplos[k] == 'gotoP'):
            PCPila.append(PC)
            PilaEje.append(PC+1)
            PC = Cuadruplos[k+3]
            
        #Read de las variables -------------------------------------------
        elif(Cuadruplos[k] == 'Read'):
            PCPila.append(PC)
            #Operando A
            if(Cuadruplos[k+3][0] != '$'):
                Ind3=NombresTokens.index(Cuadruplos[k+3]) # Muestra error si no encuentra la viariable.
                c=float(input())
                if(TipoTokens[Ind3] == "Int"):
                    ValoresTokens[Ind3]=int(c)
                else:
                    ValoresTokens[Ind3]=c
            else:
                Ind3=RAvail.index(Cuadruplos[k+3])
                FInd3=RAvailVal[Ind3]
                c=float(input())
                if(ArrayGiganteTipo[FInd3] == "Int"):
                    ArrayGigante[FInd3]=int(c)
                else:
                    ArrayGigante[FInd3]=c
            PC = PC + 1
            
        #Printa de las variables -------------------------------------------
        elif(Cuadruplos[k] == 'Printa'):
            PCPila.append(PC)
            #Operando A
            if(Cuadruplos[k+3][0] == '$'):                
                Ind3=RAvail.index(Cuadruplos[k+3]) # Muestra error si no encuentra la viariable.                                                                                                            
                FInd3=RAvailVal[Ind3]                                                                                                                                   
                print(ArrayGigante[FInd3])
            else:
                Ind3=NombresTokens.index(Cuadruplos[k+3])
                print(ValoresTokens[Ind3])
            PC = PC + 1
        
        #Asignacion de las variables -------------------------------------------
        elif(Cuadruplos[k] == '<='):
            PCPila.append(PC)
            #Operando A
            #Avail o direcciones valores
            if(Cuadruplos[k+1] == 'T1' or Cuadruplos[k+1] == '$T1' or Cuadruplos[k+1] == 'T2' or Cuadruplos[k+1] == '$T2' or Cuadruplos[k+1] == 'T3' or Cuadruplos[k+1] == '$T3' or Cuadruplos[k+1] == 'T4' or Cuadruplos[k+1] == '$T4' or Cuadruplos[k+1] == 'T5' or Cuadruplos[k+1] == '$T5' or Cuadruplos[k+1] == 'T6' or Cuadruplos[k+1] == '$T6' or Cuadruplos[k+1] == 'T7' or Cuadruplos[k+1] == '$T7' or Cuadruplos[k+1] == 'T8'  or Cuadruplos[k+1] == '$T8' or Cuadruplos[k+1] == 'T9' or Cuadruplos[k+1] == '$T9' or Cuadruplos[k+1] == 'T10' or Cuadruplos[k+1] == '$T10' or Cuadruplos[k+1] == 'T11' or Cuadruplos[k+1] == 'T12' or Cuadruplos[k+1] == 'T13' or Cuadruplos[k+1] == 'T14' or Cuadruplos[k+1] == 'T15' or Cuadruplos[k+1] == 'T16' or Cuadruplos[k+1] == 'T17' or Cuadruplos[k+1] == 'T18' or Cuadruplos[k+1] == 'T19' or Cuadruplos[k+1] == 'T20'or Cuadruplos[k+1] == 'T21' or Cuadruplos[k+1] == 'T22' or Cuadruplos[k+1] == 'T23' or Cuadruplos[k+1] == 'T24' or Cuadruplos[k+1] == 'T25' or Cuadruplos[k+1] == 'T26' or Cuadruplos[k+1] == 'T27' or Cuadruplos[k+1] == 'T28' or Cuadruplos[k+1] == 'T29' or Cuadruplos[k+1] == 'T30' or Cuadruplos[k+1] == '$T11' or Cuadruplos[k+1] == '$T12' or Cuadruplos[k+1] == '$T13' or Cuadruplos[k+1] == '$T14' or Cuadruplos[k+1] == '$T15' or Cuadruplos[k+1] == '$T16' or Cuadruplos[k+1] == '$T17' or Cuadruplos[k+1] == '$T18' or Cuadruplos[k+1] == '$T19' or Cuadruplos[k+1] == '$T20' or Cuadruplos[k+1] == '$T21' or Cuadruplos[k+1] == '$T22' or Cuadruplos[k+1] == '$T23' or Cuadruplos[k+1] == '$T24' or Cuadruplos[k+1] == '$T25' or Cuadruplos[k+1] == '$T26' or Cuadruplos[k+1] == '$T27' or Cuadruplos[k+1] == '$T28' or Cuadruplos[k+1] == '$T29' or Cuadruplos[k+1] == '$T30'):
                if(Cuadruplos[k+1][0] != '$'):
                    Ind1=Avail.index(Cuadruplos[k+1])
                    a=AvailNum[Ind1]

                else:
                    Ind1=RAvail.index(Cuadruplos[k+1])
                    pos=RAvailVal[Ind1]
                    if(ArrayGiganteTipo[pos] == "Int"):
                        a=int(ArrayGigante[pos])
                    else:
                        a= float(ArrayGigante[pos])

            #Buscar en nombre tokens
            else:
                p=0
                j=0
                q=len(NombresTokens)
                while(p<q):
                    if(Cuadruplos[k+1] == NombresTokens[p]):
                        j=1
                        break
                    else:
                        p=p+1
                    
                    
                if(j == 1):
                    Ind1 = p
                    if(TipoTokens[Ind1] == "Int"):
                        a= int(ValoresTokens[Ind1])
                    elif(TipoTokens[Ind1] == "Flo"):
                        a= float(ValoresTokens[Ind1])

                else:
                    a=float(Cuadruplos[k+1])
            
            #Resultado
            if(Cuadruplos[k+3] == 'T1' or Cuadruplos[k+3] == '$T1' or Cuadruplos[k+3] == 'T2' or Cuadruplos[k+3] == '$T2' or Cuadruplos[k+3] == 'T3' or Cuadruplos[k+3] == '$T3' or Cuadruplos[k+3] == 'T4' or Cuadruplos[k+3] == '$T4' or Cuadruplos[k+3] == 'T5' or Cuadruplos[k+3] == '$T5' or Cuadruplos[k+3] == 'T6' or Cuadruplos[k+3] == '$T6' or Cuadruplos[k+3] == 'T7' or Cuadruplos[k+3] == '$T7' or Cuadruplos[k+3] == 'T8'  or Cuadruplos[k+3] == '$T8' or Cuadruplos[k+3] == 'T9' or Cuadruplos[k+3] == '$T9' or Cuadruplos[k+3] == 'T10' or Cuadruplos[k+3] == '$T10' or Cuadruplos[k+3] == 'T11' or Cuadruplos[k+3] == 'T12' or Cuadruplos[k+3] == 'T13' or Cuadruplos[k+3] == 'T14' or Cuadruplos[k+3] == 'T15' or Cuadruplos[k+3] == 'T16' or Cuadruplos[k+3] == 'T17' or Cuadruplos[k+3] == 'T18' or Cuadruplos[k+3] == 'T19' or Cuadruplos[k+3] == 'T20'or Cuadruplos[k+3] == 'T21' or Cuadruplos[k+3] == 'T22' or Cuadruplos[k+3] == 'T23' or Cuadruplos[k+3] == 'T24' or Cuadruplos[k+3] == 'T25' or Cuadruplos[k+3] == 'T26' or Cuadruplos[k+3] == 'T27' or Cuadruplos[k+3] == 'T28' or Cuadruplos[k+3] == 'T29' or Cuadruplos[k+3] == 'T30' or Cuadruplos[k+3] == '$T11' or Cuadruplos[k+3] == '$T12' or Cuadruplos[k+3] == '$T13' or Cuadruplos[k+3] == '$T14' or Cuadruplos[k+3] == '$T15' or Cuadruplos[k+3] == '$T16' or Cuadruplos[k+3] == '$T17' or Cuadruplos[k+3] == '$T18' or Cuadruplos[k+3] == '$T19' or Cuadruplos[k+3] == '$T20' or Cuadruplos[k+3] == '$T21' or Cuadruplos[k+3] == '$T22' or Cuadruplos[k+3] == '$T23' or Cuadruplos[k+3] == '$T24' or Cuadruplos[k+3] == '$T25' or Cuadruplos[k+3] == '$T26' or Cuadruplos[k+3] == '$T27' or Cuadruplos[k+3] == '$T28' or Cuadruplos[k+3] == '$T29' or Cuadruplos[k+3] == '$T30'):
                if(Cuadruplos[k+3][0] != '$'):
                    Ind3=Avail.index(Cuadruplos[k+3])
                    c = a
                    AvailNum[Ind3]=c
                else:
                    Pos=RAvail.index(Cuadruplos[k+3])
                    d=RAvailVal[Pos]
                    if(ArrayGiganteTipo[d] == "Int"):
                        ArrayGigante[d]=int(a)
                    else:
                        ArrayGigante[d]=float(a)
            else:
                Ind3=NombresTokens.index(Cuadruplos[k+3]) # Muestra error si no encuentra la viariable.
                c= a
                if(TipoTokens[Ind3] == "Int"):
                    ValoresTokens[Ind3]=int(c)
                else:
                    ValoresTokens[Ind3]=c
            PC = PC + 1

        #Goto false de los statements-------------------------------------------
        elif(Cuadruplos[k] == 'gotoF'):
            PCPila.append(PC)
            PC = Cuadruplos[k+3]
         
        #Return de los metodos -------------------------------------------
        elif(Cuadruplos[k] == 'Return'):
            PCPila.append(PC)
            PC = PilaEje.pop()

    #Extra !!!!!
##    print('Tabla de Simbolos','\nNombres: ',NombresTokens,'\nValores: ',ValoresTokens,'\nTipos: ',TipoTokens)
##    print(AvailNum)
##    print(Avail)
##    print(AvailBool)
##    print(PCPila)
##    print(ArrayGigante)
##    print(ArrayGiganteTipo)
##    # **********************************************************************************Fin de ejecucion*******************************************************************
            
# Tipos de variables
def p_V(p):
    '''
    V : V Tipo_M
      | V Tipo_A
      | empty 
    '''
    
#Declaracion para variables simples.
def p_Tipo_M(p):
    '''
    Tipo_M : INT ID ASIGNACION INTEGER SEMICOLON 
           | FLO ID ASIGNACION FLOAT SEMICOLON
    '''
    NombresTokens.append(p[2])
    TipoTokens.append(p[1])
    ValoresTokens.append(p[4])

#Declaracion de las variables complejas como vectores y matrices.
def p_Tipo_A(p):  
    '''
    Tipo_A : INT Arreglo2 ID TAUXI LCORCHETE INTEGER TTAUXI RCORCHETE LCORCHETE INTEGER TTTAUXI RCORCHETE SEMICOLON
           | FLO Arreglo2 ID TAUXI LCORCHETE INTEGER TTAUXI RCORCHETE LCORCHETE INTEGER TTTAUXI RCORCHETE SEMICOLON
           | INT Arreglo1 ID TAUXI LCORCHETE INTEGER TTAUXI RCORCHETE SEMICOLON
           | FLO Arreglo1 ID TAUXI LCORCHETE INTEGER TTAUXI RCORCHETE SEMICOLON
    '''
#Almacenando los valores que nos serviran para los cuadruplos de las variables dimensionadas
def p_TAUXI(p):
    '''
    TAUXI : 
    '''
    VariablesDimensionadas.append(p[-1])
    global A
    global Base
    A = 1 
    Bases.append(Base)
    d.append(str(len(VariablesDimensionadas)-1)+".")
    m.append(str(len(VariablesDimensionadas)-1)+".")
    if(p[-2] == "Arr1_"):
        VariablesDimensionadasTipo.append("Arr1_")
    else:
        VariablesDimensionadasTipo.append("Arr2_")
    
def p_TTAUXI(p):
    '''
    TTAUXI : 
    '''
    global A
    global Base
    A=A*int(p[-1])
    d.append(p[-1])
    if(p[-5] == "Arr1_"):
        As.append(A)
        m.append(Base)
        Base=Base+A
        m.append("-")
        d.append("-")
        #Generando los arreglos
        j=0
        while(j < A):
            if(p[-6] == "Int"):
               ArrayGigante.append("0")
               ArrayGiganteTipo.append("Int")

            else:
               ArrayGigante.append("0.0")
               ArrayGiganteTipo.append("Flo")
            j=j+1
        
        
def p_TTTAUXI(p):
    '''
    TTTAUXI : 
    '''
    global A
    global Base
    A=A*int(p[-1])
    As.append(A)
    d.append(p[-1])
    e=VariablesDimensionadas.index(p[-8])
    f=d.index(str(e)+".")
    g=int(f)+1
    mn= int(A/int(d[g]))
    m.append(mn)
    m.append(Base)
    Base=Base+A
    m.append("-")
    d.append("-")
    #Generando los arreglos
    j=0
    while(j < A):
        if(p[-10] == "Int"):
            ArrayGigante.append("0")
            ArrayGiganteTipo.append("Int")
            
        else:
            ArrayGigante.append("0.0")
            ArrayGiganteTipo.append("Flo")
        j=j+1

#MainBlock
def p_MainBlock(p):
    '''
        MainBlock : MAIN LPAREN RPAREN PUNTOS Statements ENDMAIN SEMICOLON
    '''

#MethodsBlock
def p_MethodsBlock(p):
    '''
        MethodsBlock : METHOD ID MAUXI LPAREN RPAREN PUNTOS Statements ENDMETHOD SEMICOLON MMAUXI MethodsBlock
                     | empty
    '''
#Generandos los valores de directorio de procedimientos.
def p_MAUXI(p):
    '''
        MAUXI : 
    '''
    NombresProcedimientos.append(p[-1])
    CuadruplosProcedimientos.append(ContadorCuadruplos+1)

def p_MMAUXI(p):
    '''
        MMAUXI : 
    '''
    global ContadorCuadruplos
    ContadorCuadruplos=ContadorCuadruplos+1
    Cuadruplos.append("Return")
    Cuadruplos.append(" ")
    Cuadruplos.append(" ")
    Cuadruplos.append(" ")
    
#_________________________________________________________________________________________ Statements____________________________________________________________________________
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
    
def p_SVar(p):
    '''
        SVar : ID VAUXI LCORCHETE VVAUXI expression VVVAUXI RCORCHETE 
             | ID VAUXI LCORCHETE VVAUXI expression VVVAUXI RCORCHETE LCORCHETE expression VVVVAUXI RCORCHETE
    '''

def p_VAUXI(p):
    '''
        VAUXI : 
    '''
    Operandos.append(p[-1]) #Push ID to Operandos    
def p_VVAUXI(p):
    '''
        VVAUXI : 
    '''
    VariablesDimensionadas.index(p[-3])#Bucanso el indice del arreglo
    Operandos.pop()
def p_VVVAUXI(p):
    '''
        VVVAUXI : 
    '''
    a=VariablesDimensionadas.index(p[-5])#Bucanso el indice del arreglo
    f=d.index(str(a)+".")
    g=int(f)+1
    r=int(d[g])-1
    #Para unidimensionales
    if (VariablesDimensionadasTipo[a] == "Arr1_"):
        b=Operandos.pop()
        #Cuadruplo de ver
        Cuadruplos.append("ver")
        Cuadruplos.append(b)
        Cuadruplos.append("0")  
        Cuadruplos.append(r)
        Operandos.append(b)
        global ContadorCuadruplos
        ContadorCuadruplos=ContadorCuadruplos+1
        #Cuadruplo (Si es la ultima dimension)
        c=Operandos.pop()
        Cuadruplos.append("+")
        Cuadruplos.append(c)
        Cuadruplos.append(m[g])
        e=Avail.pop(0)
        Cuadruplos.append("$"+str(e))
        Operandos.append("$"+str(e))
        ContadorCuadruplos=ContadorCuadruplos+1
        if(c == 'T1' or c == 'T2' or c == 'T3' or c == 'T4' or c == 'T5' or c == 'T6' or c == 'T7' or c == 'T8' or c == 'T9' or c == 'T10' or c == 'T11' or c == 'T12' or c == 'T13' or c == 'T14' or c == 'T15' or c == 'T16' or c == 'T17' or c == 'T18' or c == 'T19' or c == 'T20' or c == 'T21' or c == 'T22' or c == 'T23' or c == 'T24' or c == 'T25' or c == 'T26' or c == 'T27' or c == 'T28' or c == 'T29' or c == 'T30'):
            Avail.append(c)
    #Para bidimensionales
    else:
        b=Operandos.pop()
        #Cuadruplo de ver
        Cuadruplos.append("ver")
        Cuadruplos.append(b)
        Cuadruplos.append("0")  
        Cuadruplos.append(r)
        Operandos.append(b)
        ContadorCuadruplos=ContadorCuadruplos+1
        #Cuadruplos (Si no es la ultima dimension)
        k=Operandos.pop()
        Cuadruplos.append("*")
        Cuadruplos.append(k)
        Cuadruplos.append(m[g])
        l=Avail.pop(0)
        Cuadruplos.append(l)
        Operandos.append(l)
        ContadorCuadruplos=ContadorCuadruplos+1
        if(k == 'T1' or k == 'T2' or k == 'T3' or k == 'T4' or k == 'T5' or k == 'T6' or k == 'T7' or k == 'T8' or k == 'T9' or k == 'T10' or k == 'T11' or k == 'T12' or k == 'T13' or k == 'T14' or k == 'T15' or k == 'T16' or k == 'T17' or k == 'T18' or k == 'T19' or k == 'T20' or k == 'T21' or k == 'T22' or k == 'T23' or k == 'T24' or k == 'T25' or k == 'T26' or k == 'T27' or k == 'T28' or k == 'T29' or k == 'T30'):
            Avail.append(k)
            
def p_VVVVAUXI(p):
    '''
        VVVVAUXI : 
    '''
    a=VariablesDimensionadas.index(p[-9])#Bucanso el indice del arreglo
    f=d.index(str(a)+".")
    g=int(f)+2
    r=int(d[g])-1
    #Segundo indice
    u=Operandos.pop()
    #Cuadruplo de ver
    Cuadruplos.append("ver")
    Cuadruplos.append(u)
    Cuadruplos.append("0")  
    Cuadruplos.append(r)
    Operandos.append(u)
    global ContadorCuadruplos
    ContadorCuadruplos=ContadorCuadruplos+1
    #Si i mayor a 1
    Op2=Operandos.pop()
    Op1=Operandos.pop()
    Cuadruplos.append("+")
    Cuadruplos.append(Op2)
    Cuadruplos.append(Op1)
    h=Avail.pop(0)
    Cuadruplos.append(h)
    Operandos.append(h)
    ContadorCuadruplos=ContadorCuadruplos+1
    if(Op1 == 'T1' or Op1 == 'T2' or Op1 == 'T3' or Op1 == 'T4' or Op1 == 'T5' or Op1 == 'T6' or Op1 == 'T7' or Op1 == 'T8' or Op1 == 'T9' or Op1 == 'T10' or Op1 == 'T11' or Op1 == 'T12' or Op1 == 'T13' or Op1 == 'T14' or Op1 == 'T15' or Op1 == 'T16' or Op1 == 'T17' or Op1 == 'T18' or Op1 == 'T19' or Op1 == 'T20' or Op1 == 'T21' or Op1 == 'T22' or Op1 == 'T23' or Op1 == 'T24' or Op1 == 'T25' or Op1 == 'T26' or Op1 == 'T27' or Op1 == 'T28' or Op1 == 'T29' or Op1 == 'T30'):
        Avail.append(Op1)
    if(Op2 == 'T1' or Op2 == 'T2' or Op2 == 'T3' or Op2 == 'T4' or Op2 == 'T5' or Op2 == 'T6' or Op2 == 'T7' or Op2 == 'T8' or Op2 == 'T9' or Op2 == 'T10' or Op2 == 'T11' or Op2 == 'T12' or Op2 == 'T13' or Op2 == 'T14' or Op2 == 'T15' or Op2 == 'T16' or Op2 == 'T17' or Op2 == 'T18' or Op2 == 'T19' or Op2 == 'T20' or Op2 == 'T21' or Op2 == 'T22' or Op2 == 'T23' or Op2 == 'T24' or Op2 == 'T25' or Op2 == 'T26' or Op2 == 'T27' or Op2 == 'T28' or Op2 == 'T29' or Op2 == 'T30'):
        Avail.append(Op2)
    #Cuadruplo (Si es la ultima dimension)
    c=Operandos.pop()
    Cuadruplos.append("+")
    Cuadruplos.append(c)
    Cuadruplos.append(m[g])
    e=Avail.pop(0)
    Cuadruplos.append("$"+str(e))
    Operandos.append("$"+str(e))
    ContadorCuadruplos=ContadorCuadruplos+1
    if(c == 'T1' or c == 'T2' or c == 'T3' or c == 'T4' or c == 'T5' or c == 'T6' or c == 'T7' or c == 'T8' or c == 'T9' or c == 'T10' or c == 'T11' or c == 'T12' or c == 'T13' or c == 'T14' or c == 'T15' or c == 'T16' or c == 'T17' or c == 'T18' or c == 'T19' or c == 'T20' or c == 'T21' or c == 'T22' or c == 'T23' or c == 'T24' or c == 'T25' or c == 'T26' or c == 'T27' or c == 'T28' or c == 'T29' or c == 'T30'):
        Avail.append(c)
    
##Cuadruplos Printa  
def p_PAUXI(p):
    '''
        PAUXI :
    '''
    global ContadorCuadruplos
    ContadorCuadruplos=ContadorCuadruplos+1
    Cuadruplos.append("Printa")
    Cuadruplos.append(" ")
    Cuadruplos.append(" ")
    Cuadruplos.append(p[-1])

def p_PAUXIV(p):
    '''
        PAUXIV :
    '''
    global ContadorCuadruplos
    ContadorCuadruplos=ContadorCuadruplos+1
    a=Operandos.pop()
    Cuadruplos.append("Printa")
    Cuadruplos.append(" ")
    Cuadruplos.append(" ")
    Cuadruplos.append(a)
    
##Cuadruplos Read
def p_RAUXI(p):
    '''
        RAUXI :
    '''
    global ContadorCuadruplos
    ContadorCuadruplos=ContadorCuadruplos+1
    Cuadruplos.append("Read")
    Cuadruplos.append(" ")
    Cuadruplos.append(" ")
    Cuadruplos.append(p[-1])

def p_RAUXIV(p):
    '''
        RAUXIV :
    '''
    global ContadorCuadruplos
    ContadorCuadruplos=ContadorCuadruplos+1
    a=Operandos.pop()
    Cuadruplos.append("Read")
    Cuadruplos.append(" ")
    Cuadruplos.append(" ")
    Cuadruplos.append(a)
    
##Cuadruplos para Call
def p_CAUXI(p):
    '''
        CAUXI :
    '''
    global ContadorCuadruplos
    ContadorCuadruplos=ContadorCuadruplos+1
    Cuadruplos.append("gotoP")
    Cuadruplos.append(" ")
    Cuadruplos.append(" ")
    Cuadruplos.append(CuadruplosProcedimientos[NombresProcedimientos.index(p[-1])])
    
###Cuadruplos For
def p_FAUXI(p):
    '''FAUXI : '''
    Operandos.append(p[-3])
    Operandos.append(" ")
    Operandos.append(p[-4])
    a=Operandos.pop(1)
    Cuadruplos.append(a)
    b=Operandos.pop(0)
    Cuadruplos.append(b)
    c=Operandos.pop(0)
    Cuadruplos.append(c)
    d=Operandos.pop(0)
    Cuadruplos.append(d)
    if(b == 'T1' or b == 'T2' or b == 'T3' or b == 'T4' or b == 'T5' or b == 'T6' or b == 'T7' or b == 'T8' or b == 'T9' or b == 'T10' or b == 'T11' or b == 'T12' or b == 'T13' or b == 'T14' or b == 'T15' or b == 'T16' or b == 'T17' or b == 'T18' or b == 'T19' or b == 'T20' or b == 'T21' or b == 'T22' or b == 'T23' or b == 'T24' or b == 'T25' or b == 'T26' or b == 'T27' or b == 'T28' or b == 'T29' or b == 'T30'):
        Avail.append(b)
    global ContadorCuadruplos
    ContadorCuadruplos=ContadorCuadruplos+1
        
def p_FFAUXI(p):
    '''FFAUXI : '''
    global ContadorCuadruplos
    ContadorCuadruplos=ContadorCuadruplos+1
    PilaSaltos.append(ContadorCuadruplos)
    
def p_FFFAUXI(p): # Se cambio ligeramente este !!!!!
    '''FFFAUXI : '''
    a=Cuadruplos[(ContadorCuadruplos-1)*4-1]
    Cuadruplos.append("gotoF")
    Cuadruplos.append(a)
    AvailAuxi.append(a)
    Cuadruplos.append(" ")
    Cuadruplos.append(" ")
    PilaSaltos.append(ContadorCuadruplos)

def p_FFFFAUXI(p):
    '''FFFFAUXI : '''
    global ContadorCuadruplos                # El incremento de la variable.
    ContadorCuadruplos=ContadorCuadruplos+1  
    Cuadruplos.append("+")
    Cuadruplos.append(p[-11])
    Cuadruplos.append("1")
    d=Avail.pop(0)
    Cuadruplos.append(d)
    ContadorCuadruplos=ContadorCuadruplos+1  
    Cuadruplos.append("<=")
    Cuadruplos.append(d)
    Cuadruplos.append(" ")
    Cuadruplos.append(p[-11])
    Avail.append(d)
    ContadorCuadruplos=ContadorCuadruplos+1
    a=PilaSaltos.pop()
    b=PilaSaltos.pop()
    a=a*4-1
    Cuadruplos[a] = ContadorCuadruplos+1
    Cuadruplos.append("goto")
    Cuadruplos.append(" ")
    Cuadruplos.append(" ")
    Cuadruplos.append(b)
    
###Cuadruplos While
def p_WAUXI(p):
    '''WAUXI : '''
    global ContadorCuadruplos
    ContadorCuadruplos=ContadorCuadruplos+1
    PilaSaltos.append(ContadorCuadruplos)

def p_WWAUXI(p):
    '''WWAUXI : '''
    a=Cuadruplos[(ContadorCuadruplos-1)*4-1]
    Cuadruplos.append("gotoF")
    Cuadruplos.append(a)
    AvailAuxi.append(a)
    Cuadruplos.append(" ")
    Cuadruplos.append(" ")
    PilaSaltos.append(ContadorCuadruplos)

def p_WWWAUXI(p):
    '''WWWAUXI : '''
    global ContadorCuadruplos
    ContadorCuadruplos=ContadorCuadruplos+1
    a=PilaSaltos.pop()
    b=PilaSaltos.pop()
    a=a*4-1
    Cuadruplos[a] = ContadorCuadruplos+1
    Cuadruplos.append("goto")
    Cuadruplos.append(" ")
    Cuadruplos.append(" ")
    Cuadruplos.append(b)
    
###Cuadruplos IF
def p_IAUXI(p):
    '''IAUXI : '''
    global ContadorCuadruplos
    ContadorCuadruplos=ContadorCuadruplos+1
    a=Cuadruplos[(ContadorCuadruplos-1)*4-1]
    Cuadruplos.append("gotoF")
    Cuadruplos.append(a)
    AvailAuxi.append(a)
    Cuadruplos.append(" ")
    Cuadruplos.append(" ")
    PilaSaltos.append(ContadorCuadruplos)
    
def p_IIAUXI(p):
    '''IIAUXI : '''
    global ContadorCuadruplos
    ContadorCuadruplos=ContadorCuadruplos+1
    a=PilaSaltos.pop()
    a=a*4 -1
    Cuadruplos[a] = ContadorCuadruplos+1
    Cuadruplos.append("goto")
    Cuadruplos.append(" ")
    Cuadruplos.append(" ")
    Cuadruplos.append(" ")
    PilaSaltos.append(ContadorCuadruplos)

def p_IIIAUXI(p):
    '''IIIAUXI : '''
    a=PilaSaltos.pop()
    a=a*4 -1
    Cuadruplos[a] = ContadorCuadruplos+1
    
#Ultimo detalle para transdormar a cuadruplos (Para la asignacion)
def p_Auxi(p):
    '''Auxi : '''
    Operandos.append(p[-3])
    Operandos.append(" ")
    Operandos.append(p[-4])
    a=Operandos.pop(1) #Asignacion (<=)
    Cuadruplos.append(a)
    b=Operandos.pop(0) 
    Cuadruplos.append(b)
    c=Operandos.pop(0)
    Cuadruplos.append(c)
    d=Operandos.pop(0)
    Cuadruplos.append(d)
    if(b == 'T1' or b == 'T2' or b == 'T3' or b == 'T4' or b == 'T5' or b == 'T6' or b == 'T7' or b == 'T8' or b == 'T9' or b == 'T10' or b == 'T11' or b == 'T12' or b == 'T13' or b == 'T14' or b == 'T15' or b == 'T16' or b == 'T17' or b == 'T18' or b == 'T19' or b == 'T20' or b == 'T21' or b == 'T22' or b == 'T23' or b == 'T24' or b == 'T25' or b == 'T26' or b == 'T27' or b == 'T28' or b == 'T29' or b == 'T30'):
        Avail.append(b)
    global ContadorCuadruplos
    ContadorCuadruplos=ContadorCuadruplos+1

def p_AuxiV(p):
    '''AuxiV : '''
    Operandos.append(p[-3])
    Operandos.append(" ")
    a=Operandos.pop(2) #Asignacion (<=)
    Cuadruplos.append(a)
    b=Operandos.pop(1) 
    Cuadruplos.append(b)
    c=Operandos.pop(1)
    Cuadruplos.append(c)
    d=Operandos.pop(0)
    Cuadruplos.append(d)
    if(b == 'T1' or b == 'T2' or b == 'T3' or b == 'T4' or b == 'T5' or b == 'T6' or b == 'T7' or b == 'T8' or b == 'T9' or b == 'T10' or b == 'T11' or b == 'T12' or b == 'T13' or b == 'T14' or b == 'T15' or b == 'T16' or b == 'T17' or b == 'T18' or b == 'T19' or b == 'T20' or b == 'T21' or b == 'T22' or b == 'T23' or b == 'T24' or b == 'T25' or b == 'T26' or b == 'T27' or b == 'T28' or b == 'T29' or b == 'T30'):
        Avail.append(b)
    global ContadorCuadruplos
    ContadorCuadruplos=ContadorCuadruplos+1
    
# Condicion de If, While, For
def p_Comparison(p): 
    '''
    Comparison :  ID IGUALDADCOM ID
               |  ID MAYORCOM ID
               |  ID MENORCOM ID

    '''
    if(p[2] == '='):
        Cuadruplos.append(p[2]) # El signo del cuadruplo
        Cuadruplos.append(p[1]) # Operando 1
        Cuadruplos.append(p[3]) # Operando 2
        a=Avail.pop(0)           # Temporal donde se guarda el resultado
        Cuadruplos.append(a)
        global ContadorCuadruplos
        ContadorCuadruplos=ContadorCuadruplos+1
    
    elif(p[2] == '>'):
        Cuadruplos.append(p[2]) # El signo del cuadruplo
        Cuadruplos.append(p[1]) # Operando 1
        Cuadruplos.append(p[3]) # Operando 2
        a=Avail.pop(0)           # Temporal donde se guarda el resultado
        Cuadruplos.append(a)
        ContadorCuadruplos=ContadorCuadruplos+1    
        
    elif(p[2] == '<'):
        Cuadruplos.append(p[2]) # El signo del cuadruplo
        Cuadruplos.append(p[1]) # Operando 1
        Cuadruplos.append(p[3]) # Operando 2
        a=Avail.pop(0)           # Temporal donde se guarda el resultado
        Cuadruplos.append(a)
        ContadorCuadruplos=ContadorCuadruplos+1
            
# Definicion de expression.
def p_expression(p):
    '''
    expression : expression PLUS S
               | S  
    '''
    if (len(p) == 4):
        Cuadruplos.append('+')  
        a=Operandos.pop()
        Cuadruplos.append(a)
        b=Operandos.pop()
        Cuadruplos.append(b)
        c=Avail.pop(0)
        Operandos.append(c)
        Cuadruplos.append(c)
        if(a == 'T1' or a == 'T2' or a == 'T3' or a == 'T4' or a == 'T5' or a == 'T6' or a == 'T7' or a == 'T8' or a == 'T9' or a == 'T10' or a == 'T11' or a == 'T12' or a == 'T13' or a == 'T14' or a == 'T15' or a == 'T16' or a == 'T17' or a == 'T18' or a == 'T19' or a == 'T20' or a == 'T21' or a == 'T22' or a == 'T23' or a == 'T24' or a == 'T25' or a == 'T26' or a == 'T27' or a == 'T28' or a == 'T29' or a == 'T30'):
            Avail.append(a)
        if(b == 'T1' or b == 'T2' or b == 'T3' or b == 'T4' or b == 'T5' or b == 'T6' or b == 'T7' or b == 'T8' or b == 'T9' or b == 'T10' or b == 'T11' or b == 'T12' or b == 'T13' or b == 'T14' or b == 'T15' or b == 'T16' or b == 'T17' or b == 'T18' or b == 'T19' or b == 'T20' or b == 'T21' or b == 'T22' or b == 'T23' or b == 'T24' or b == 'T25' or b == 'T26' or b == 'T27' or b == 'T28' or b == 'T29' or b == 'T30'):
            Avail.append(b)
        global ContadorCuadruplos
        ContadorCuadruplos=ContadorCuadruplos+1

def p_S(p):
    '''
        S : S MINUS T
          | T  
    '''
    if (len(p) == 4):
        Cuadruplos.append('-')  
        a=Operandos.pop()
        Cuadruplos.append(a)
        b=Operandos.pop()
        Cuadruplos.append(b)
        c=Avail.pop(0)
        Operandos.append(c)
        Cuadruplos.append(c)
        if(a == 'T1' or a == 'T2' or a == 'T3' or a == 'T4' or a == 'T5' or a == 'T6' or a == 'T7' or a == 'T8' or a == 'T9' or a == 'T10' or a == 'T11' or a == 'T12' or a == 'T13' or a == 'T14' or a == 'T15' or a == 'T16' or a == 'T17' or a == 'T18' or a == 'T19' or a == 'T20' or a == 'T21' or a == 'T22' or a == 'T23' or a == 'T24' or a == 'T25' or a == 'T26' or a == 'T27' or a == 'T28' or a == 'T29' or a == 'T30'):
            Avail.append(a)
        if(b == 'T1' or b == 'T2' or b == 'T3' or b == 'T4' or b == 'T5' or b == 'T6' or b == 'T7' or b == 'T8' or b == 'T9' or b == 'T10' or b == 'T11' or b == 'T12' or b == 'T13' or b == 'T14' or b == 'T15' or b == 'T16' or b == 'T17' or b == 'T18' or b == 'T19' or b == 'T20' or b == 'T21' or b == 'T22' or b == 'T23' or b == 'T24' or b == 'T25' or b == 'T26' or b == 'T27' or b == 'T28' or b == 'T29' or b == 'T30'):
            Avail.append(b)
        global ContadorCuadruplos
        ContadorCuadruplos=ContadorCuadruplos+1
        
def p_T(p):
    '''
    T : T TIMES F
      | F 
    '''
    if (len(p) == 4):
        Cuadruplos.append('*')
        a=Operandos.pop()
        Cuadruplos.append(a)
        b=Operandos.pop()
        Cuadruplos.append(b)
        c=Avail.pop(0)
        Operandos.append(c)
        Cuadruplos.append(c)
        if(a == 'T1' or a == 'T2' or a == 'T3' or a == 'T4' or a == 'T5' or a == 'T6' or a == 'T7' or a == 'T8' or a == 'T9' or a == 'T10' or a == 'T11' or a == 'T12' or a == 'T13' or a == 'T14' or a == 'T15' or a == 'T16' or a == 'T17' or a == 'T18' or a == 'T19' or a == 'T20' or a == 'T21' or a == 'T22' or a == 'T23' or a == 'T24' or a == 'T25' or a == 'T26' or a == 'T27' or a == 'T28' or a == 'T29' or a == 'T30'):
            Avail.append(a)
        if(b == 'T1' or b == 'T2' or b == 'T3' or b == 'T4' or b == 'T5' or b == 'T6' or b == 'T7' or b == 'T8' or b == 'T9' or b == 'T10' or b == 'T11' or b == 'T12' or b == 'T13' or b == 'T14' or b == 'T15' or b == 'T16' or b == 'T17' or b == 'T18' or b == 'T19' or b == 'T20' or b == 'T21' or b == 'T22' or b == 'T23' or b == 'T24' or b == 'T25' or b == 'T26' or b == 'T27' or b == 'T28' or b == 'T29' or b == 'T30'):
            Avail.append(b)
        global ContadorCuadruplos
        ContadorCuadruplos=ContadorCuadruplos+1

def p_F(p):
    '''
    F : F DIVIDE R
      | R 
    '''
    if (len(p) == 4):
        Cuadruplos.append('/')
        a=Operandos.pop()
        Cuadruplos.append(a)
        b=Operandos.pop()
        Cuadruplos.append(b)
        c=Avail.pop(0)
        Operandos.append(c)
        Cuadruplos.append(c)
        if(a == 'T1' or a == 'T2' or a == 'T3' or a == 'T4' or a == 'T5' or a == 'T6' or a == 'T7' or a == 'T8' or a == 'T9' or a == 'T10' or a == 'T11' or a == 'T12' or a == 'T13' or a == 'T14' or a == 'T15' or a == 'T16' or a == 'T17' or a == 'T18' or a == 'T19' or a == 'T20' or a == 'T21' or a == 'T22' or a == 'T23' or a == 'T24' or a == 'T25' or a == 'T26' or a == 'T27' or a == 'T28' or a == 'T29' or a == 'T30'):
            Avail.append(a)
        if(b == 'T1' or b == 'T2' or b == 'T3' or b == 'T4' or b == 'T5' or b == 'T6' or b == 'T7' or b == 'T8' or b == 'T9' or b == 'T10' or b == 'T11' or b == 'T12' or b == 'T13' or b == 'T14' or b == 'T15' or b == 'T16' or b == 'T17' or b == 'T18' or b == 'T19' or b == 'T20' or b == 'T21' or b == 'T22' or b == 'T23' or b == 'T24' or b == 'T25' or b == 'T26' or b == 'T27' or b == 'T28' or b == 'T29' or b == 'T30'):
            Avail.append(b)
        global ContadorCuadruplos
        ContadorCuadruplos=ContadorCuadruplos+1
        
def p_R(p):
    '''
    R : ID
      | SVar
      | INTEGER
      | FLOAT
      | LPAREN expression RPAREN
    '''
    if(p[1] != '(' and p[1] != None):
        Operandos.append(p[1])
#_________________________________________________________________________________________ End Statements____________________________________________________________________________
    
def p_error(t):
    print("Syntax error at '%s'" % t.value)
    global Error
    Error = True # Se manda señal de fallo
    
def p_empty(p):
    '''
    empty :
    '''
    p[0] = None
    
#Build the yacc
parser = yacc.yacc()
#Input data
data = input()
#Lexer reading data
lexer.input(data)
#Yacc reading data
parser.parse(data)
