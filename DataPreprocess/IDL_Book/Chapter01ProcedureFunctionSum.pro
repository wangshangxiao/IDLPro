; Chapter01ProcedureFunctionSum.pro
PRO Chapter01ProcedureFunctionSum
READ, PROMPT="������X = ?", x
READ, PROMPT="������Y = ?", y
Sum = Chapter01FunctionSum(x, y)
PRINT, x , "  +  " , y , "  = ", Sum
END
