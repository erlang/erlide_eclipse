package org.erlide.cover.views.model;

import java.io.Serializable;

/**
 * Interaface for objects in coverage statistics tree
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 * 
 */
public interface ICoverageObject extends Serializable {

    public ICoverageObject findChild(String name);

    public String getHtmlPath();

    public void setHtmlPath(final String htmlPath);

    public ICoverageObject getPrevSiblingTo(String name);

    public ICoverageObject getNextSiblingTo(String name);

    public ICoverageObject getParent();

    public void setParent(ICoverageObject parent);

    public void addChild(String name, ICoverageObject child);

    public void removeChild(String name);

    public void removeAllChildren();

    public ICoverageObject[] getChildren();

    public boolean hasChildren();

    public ObjectType getType();

    public String getLabel();

    public void setLabel(String label);

    public int getLinesCount();

    public void setLiniesCount(int count);

    public int getCoverCount();

    public void setCoverCount(int count);

    public double getPercentage();

    public String[] getStringArray();

    public ICoverageObject treeSearch(String name);
    
}
