package org.erlide.cover.views.model;

import java.util.LinkedList;
import java.util.List;

/**
 * Basic element of the EUnit result tree
 * 
 * @author Aleksandra Lipiec
 * 
 */
public class TestTreeObject {

    public static final int WARN = 0;
    public static final int SUCCESS = 1;
    public static final int FAILOURE = 2;
    public static final int DESCR = 3;

    private String name;
    private String description = "";
    private List<TestTreeObject> children = new LinkedList<TestTreeObject>();
    private TestTreeObject parent = null;
    private int type; // type of node

    public TestTreeObject(String name, int type) {
        this.name = name;
        this.description = name.replace("\"", "").replace("\\'", "'")
                .replaceFirst("^'", "").replaceFirst("'$", "");
        this.type = type;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public String getDescription() {
        return description;
    }

    /**
     * Adds time to the description
     * 
     * @param time
     */
    public void setTime(int time) {
        description = String
                .format("%s     [%.3f]", description, time / 1000.0);
    }

    public void addChild(TestTreeObject child) {
        child.setParent(this);
        children.add(child);
    }

    public List<TestTreeObject> getChildren() {
        return children;
    }

    public boolean hasChildren() {
        return !children.isEmpty();
    }

    public TestTreeObject getParent() {
        return parent;
    }

    private void setParent(TestTreeObject parent) {
        this.parent = parent;
    }

    public String getName() {
        return name;
    }

    public TestTreeObject findNode(String name) {
        if (this.name.equals(name))
            return this;
        TestTreeObject result;
        for (TestTreeObject obj : children) {
            if ((result = obj.findNode(name)) != null)
                return result;
        }
        return null;
    }

    public int getType() {
        return type;
    }
    
    //updates type
    public void updateType() {
        if (children.size() == 0)
            return;
        
        type = TestTreeObject.SUCCESS;
        
        for(TestTreeObject child : children) {
            if(child.getType() != TestTreeObject.SUCCESS)
                type = TestTreeObject.WARN;
        }
    }

}
