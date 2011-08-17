package org.erlide.cover.views.model;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;

/**
 * The content provider class is responsible for providing objects to the view.
 * It can wrap existing objects in adapters or simply return objects as-is.
 * These objects may be sensitive to the current input of the view, or ignore it
 * and always show the same content (like Task List, for example).
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang.solutions.com>
 */
public class StatsTreeObject implements ICoverageObject {

    private static final long serialVersionUID = 1L;

    private final ObjectType type;

    private String label; // name
    private int all; // total line number
    private int covered; // covered line number
    private String htmlPath; // name of html file
    private String relativePath; // relative HTML path

    private ICoverageObject parent;
    private final Map<String, ICoverageObject> children;

    public StatsTreeObject(final ObjectType type) {
        this.type = type;
        children = new HashMap<String, ICoverageObject>();
    }

    public StatsTreeObject(final ICoverageObject parent, final ObjectType type) {
        this(type);
        this.parent = parent;
    }

    public StatsTreeObject(final String label, final int all,
            final int covered, final ObjectType type) {
        this(type);
        this.label = label;
        this.all = all;
        this.covered = covered;
    }

    public ObjectType getType() {
        return type;
    }

    public String getLabel() {
        return label;
    }

    @Override
    public String toString() {

        final StringBuffer bf = new StringBuffer();
        bf.append(label).append(" ").append(all).append(" ").append(covered)
                .append(" ").append(getPercentage()).append('\n');

        for (final ICoverageObject child : children.values()) {
            bf.append('\t').append(child.toString()).append('\n');
        }

        return bf.toString();
    }

    public Object getAdapter(@SuppressWarnings("rawtypes") final Class key) {
        return null;
    }

    public void setParent(final ICoverageObject parent) {
        this.parent = parent;
    }

    public ICoverageObject getParent() {
        return parent;
    }

    public void addChild(final String name, final ICoverageObject child) {
        children.put(name, child);
        child.setParent(this);
    }

    public void removeChild(final String name) {
        children.remove(name);
    }

    public ICoverageObject[] getChildren() {
        return children.values().toArray(new ICoverageObject[0]);
    }

    public boolean hasChildren() {
        return !children.isEmpty();
    }

    public void setLabel(final String label) {
        this.label = label;
    }

    public int getLinesCount() {
        return all;
    }

    public void setLiniesCount(final int count) {
        all = count;
    }

    public int getCoverCount() {
        return covered;
    }

    public void setCoverCount(final int count) {
        covered = count;
    }

    public double getPercentage() {
        if (all > 0) {
            return covered / (double) all * 100;
        }
        return 0;
    }

    public void removeAllChildren() {
        children.clear();
    }

    public String[] getStringArray() {
        return new String[] { label, Integer.toString(all),
                Integer.toString(covered),
                String.format("%.2f", getPercentage()) };
    }

    public String getHtmlPath() {
        return htmlPath;
    }

    public void setHtmlPath(final String htmlPath) {
        this.htmlPath = htmlPath;
    }

    public ICoverageObject findChild(final String name) {
        return children.get(name);
    }

    /**
     * Returns sibbling name to the name given
     */
    public ICoverageObject getPrevSiblingTo(final String name) {
        final List<String> l = new LinkedList<String>(children.keySet());
        final ListIterator<String> it = l.listIterator(l.indexOf(name));

        if (it.hasPrevious()) {
            return children.get(it.previous());
        } else {
            return null;
        }
    }

    public ICoverageObject getNextSiblingTo(final String name) {
        final List<String> l = new LinkedList<String>(children.keySet());
        final ListIterator<String> it = l.listIterator(l.indexOf(name));

        it.next();
        if (it.hasNext()) {
            return children.get(it.next());
        } else {
            return null;
        }
    }

    public ICoverageObject treeSearch(final String name) {
        if (label.equals(name)) {
            return this;
        }
        ICoverageObject res = null;
        for (final ICoverageObject child : children.values()) {
            res = child.treeSearch(name);
            if (res != null) {
                break;
            }
        }
        return res;
    }

    public Collection<ICoverageObject> getModules() {
        final Collection<ICoverageObject> col = new HashSet<ICoverageObject>();
        if (type.equals(ObjectType.MODULE)) {
            col.add(this);
        } else if (hasChildren()) {
            for (final ICoverageObject child : children.values()) {
                col.addAll(child.getModules());
            }
        }

        return col;
    }

    public String getPercentageStringified() {
        return String.format("%.2f ", getPercentage()) + "%";
    }

    public String getRelativePath() {
        return relativePath;
    }

    public void setRelativePath(final String path) {
        relativePath = path;
    }

}
