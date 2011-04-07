package org.erlide.cover.views.model;

import java.io.Serializable;
import java.util.Collection;

/**
 * Interaface for objects in coverage statistics tree
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 * 
 */
public interface ICoverageObject extends Serializable {

    /**
     * Search children by name
     * 
     * @param name
     * @return
     */
    public ICoverageObject findChild(String name);

    /**
     * Return a path to the html report for the element
     * 
     * @return
     */
    public String getHtmlPath();

    /**
     * Return a path relative to reports directory
     * 
     * @return
     */
    public String getRelativePath();

    /**
     * Sets relative path
     * 
     * @param path
     */
    public void setRelativePath(String path);

    /**
     * Set a path to the html report for the element
     * 
     * @param htmlPath
     */
    public void setHtmlPath(final String htmlPath);

    /**
     * Find next sibling to the given child
     * 
     * @param name
     * @return
     */
    public ICoverageObject getPrevSiblingTo(String name);

    /**
     * Find previous sibling to the given child
     * 
     * @param name
     * @return
     */
    public ICoverageObject getNextSiblingTo(String name);

    /**
     * Returns element's parent
     * 
     * @return
     */
    public ICoverageObject getParent();

    /**
     * Set the elements parent
     * 
     * @param parent
     */
    public void setParent(ICoverageObject parent);

    /**
     * add a child for the element
     * 
     * @param name
     * @param child
     */
    public void addChild(String name, ICoverageObject child);

    /**
     * removes a child by name
     * 
     * @param name
     */
    public void removeChild(String name);

    /**
     * remove all children of the element
     */
    public void removeAllChildren();

    /**
     * return children table
     * 
     * @return
     */
    public ICoverageObject[] getChildren();

    /**
     * check if the element has any children
     * 
     * @return
     */
    public boolean hasChildren();

    /**
     * check the element's type
     * 
     * @return
     */
    public ObjectType getType();

    /**
     * return the element's label
     * 
     * @return
     */
    public String getLabel();

    /**
     * set the element's label
     * 
     * @param label
     */
    public void setLabel(String label);

    /**
     * return the number of the lines
     * 
     * @return
     */
    public int getLinesCount();

    /**
     * set the number of the lines (total)
     * 
     * @param count
     */
    public void setLiniesCount(int count);

    /**
     * return the number of covered lines
     * 
     * @return
     */
    public int getCoverCount();

    /**
     * set the number of covered lines
     * 
     * @param count
     */
    public void setCoverCount(int count);

    /**
     * get coverage ratio
     * 
     * @return
     */
    public double getPercentage();

    /**
     * get coverage ratio as a String (e.g. for html reports)
     * 
     * @return
     */
    public String getPercentageStringified();

    /**
     * get array of children names
     * 
     * @return
     */
    public String[] getStringArray();

    /**
     * serch the element's tree to find an element withc specified name
     * 
     * @param name
     * @return
     */
    public ICoverageObject treeSearch(String name);

    /**
     * Returns all modules from the tree if there are any
     * 
     * @return
     */
    public Collection<ICoverageObject> getModules();

}
