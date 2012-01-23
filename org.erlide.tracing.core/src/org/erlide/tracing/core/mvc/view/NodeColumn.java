package org.erlide.tracing.core.mvc.view;

/**
 * Enum describing columns in nodes table.
 * 
 * @author Piotr Dorobisz
 * 
 */
public enum NodeColumn {

    //@formatter:off
    ENABLED("Enabled", 60), 
    NODE_NAME("Name", 150), 
    TYPE("Type", 70), 
    COOKIE("Cookie", 150);
    //@formatter:on

    private final String name;
    private final int width;

    private NodeColumn(final String name, final int width) {
        this.name = name;
        this.width = width;
    }

    public static NodeColumn getByIndex(final int index) {
        for (final NodeColumn column : NodeColumn.values()) {
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
