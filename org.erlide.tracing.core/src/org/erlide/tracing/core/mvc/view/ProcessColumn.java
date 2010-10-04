package org.erlide.tracing.core.mvc.view;

/**
 * Enum describing columns in processes table.
 * 
 * @author Piotr Dorobisz
 * 
 */
public enum ProcessColumn {
    SELECTED("Selected", 60), //
    INITIAL_CALL("Initial call", 130), //
    NAME("Process name", 150), //
    NODE("Process node", 150);

    private final String name;
    private final int width;

    private ProcessColumn(String name, int width) {
        this.name = name;
        this.width = width;
    }

    /**
     * Returns enum value for given ordinal. If there is no enum with given
     * ordinal it will return <code>null</code>.
     * 
     * @param index
     * @return enum value
     */
    public static ProcessColumn getByIndex(int index) {
        for (ProcessColumn column : ProcessColumn.values()) {
            if (column.ordinal() == index)
                return column;
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
