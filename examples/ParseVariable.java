public class ParseVariable{
    @defsfn
    private SymbolTable table;
    public ParseVariable (ExpressionObject expr){
        symbols = expr.parse();
        this.table.write(symbols);
        return(this.table);
    }
}
