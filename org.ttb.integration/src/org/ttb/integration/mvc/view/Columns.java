package org.ttb.integration.mvc.view;

/**
 * Enum describing table columns.
 * 
 * @author Piotr Dorobisz
 * 
 */
public enum Columns {
    ENABLED("Enabled", 60), MODULE_NAME("Module name", 150), FUNCTION_NAME("Function name", 150);

    private final String name;
    private final int width;

    private Columns(String name, int width) {
        this.name = name;
        this.width = width;
    }

    public static Columns getByIndex(int index) {
        for (Columns column : Columns.values()) {
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
