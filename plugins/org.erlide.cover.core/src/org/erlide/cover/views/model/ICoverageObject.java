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
    ICoverageObject findChild(String name);

    /**
     * Return a path to the html report for the element
     *
     * @return
     */
    String getHtmlPath();

    /**
     * Return a path relative to reports directory
     *
     * @return
     */
    String getRelativePath();

    /**
     * Sets relative path
     *
     * @param path
     */
    void setRelativePath(String path);

    /**
     * Set a path to the html report for the element
     *
     * @param htmlPath
     */
    void setHtmlPath(final String htmlPath);

    /**
     * Find next sibling to the given child
     *
     * @param name
     * @return
     */
    ICoverageObject getPrevSiblingTo(String name);

    /**
     * Find previous sibling to the given child
     *
     * @param name
     * @return
     */
    ICoverageObject getNextSiblingTo(String name);

    /**
     * Returns element's parent
     *
     * @return
     */
    ICoverageObject getParent();

    /**
     * Set the elements parent
     *
     * @param parent
     */
    void setParent(ICoverageObject parent);

    /**
     * add a child for the element
     *
     * @param name
     * @param child
     */
    void addChild(String name, ICoverageObject child);

    /**
     * removes a child by name
     *
     * @param name
     */
    void removeChild(String name);

    /**
     * remove all children of the element
     */
    void removeAllChildren();

    /**
     * return children table
     *
     * @return
     */
    ICoverageObject[] getChildren();

    /**
     * check if the element has any children
     *
     * @return
     */
    boolean hasChildren();

    /**
     * check the element's type
     *
     * @return
     */
    ObjectType getType();

    /**
     * return the element's label
     *
     * @return
     */
    String getLabel();

    /**
     * set the element's label
     *
     * @param label
     */
    void setLabel(String label);

    /**
     * return the number of the lines
     *
     * @return
     */
    int getLinesCount();

    /**
     * set the number of the lines (total)
     *
     * @param count
     */
    void setLiniesCount(int count);

    /**
     * return the number of covered lines
     *
     * @return
     */
    int getCoverCount();

    /**
     * set the number of covered lines
     *
     * @param count
     */
    void setCoverCount(int count);

    /**
     * get coverage ratio
     *
     * @return
     */
    double getPercentage();

    /**
     * get coverage ratio as a String (e.g. for html reports)
     *
     * @return
     */
    String getPercentageStringified();

    /**
     * get array of children names
     *
     * @return
     */
    String[] getStringArray();

    /**
     * serch the element's tree to find an element withc specified name
     *
     * @param name
     * @return
     */
    ICoverageObject treeSearch(String name);

    /**
     * Returns all modules from the tree if there are any
     *
     * @return
     */
    Collection<ICoverageObject> getModules();

}
