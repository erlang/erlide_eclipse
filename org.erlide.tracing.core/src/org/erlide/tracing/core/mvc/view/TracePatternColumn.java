package org.erlide.tracing.core.mvc.view;

/**
 * Enum describing columns in trace pattern table.
 * 
 * @author Piotr Dorobisz
 * 
 */
public enum TracePatternColumn {
    //@formatter:off
    ENABLED("Enabled", 60),
    LOCAL("Local", 60),
    MODULE_NAME("Module name", 150),
    FUNCTION_NAME("Function name", 150),
    ARITY("Arity", 40),
    MATCH_SPEC("Match spec", 90);
    //@formatter:on

    private final String name;
    private final int width;

    private TracePatternColumn(final String name, final int width) {
        this.name = name;
        this.width = width;
    }

    public static TracePatternColumn getByIndex(final int index) {
        for (final TracePatternColumn column : TracePatternColumn.values()) {
            if (column.ordinal() == index) {
                return column;
            }
        }
        return null;
    }

    public String getName() {
        return name;
    }

    public int getWidth() {
        return width;
    }
}
