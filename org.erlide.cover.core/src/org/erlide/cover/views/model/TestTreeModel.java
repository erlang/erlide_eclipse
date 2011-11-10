package org.erlide.cover.views.model;

import java.util.LinkedList;
import java.util.List;

/**
 * A simple model for collecting EUnit tests results
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 * 
 */
public class TestTreeModel {

    private static TestTreeModel model;

    private final List<TestTreeObject> rootLevel; // test result tree

    private int pass; // number of passed tests
    private int fail; // number of failed tests
    private int skip; // number of skipped tests
    private int cancel; // number of canceled tests

    private TestTreeModel() {
        rootLevel = new LinkedList<TestTreeObject>();
    }

    public static synchronized TestTreeModel getInstance() {
        if (model == null) {
            model = new TestTreeModel();
        }
        return model;
    }

    /**
     * The basic level of eunit tree
     * 
     * @return root level
     */
    public List<TestTreeObject> getRootLevel() {
        return rootLevel;
    }

    /**
     * Add a new node to the basic level
     * 
     * @param obj
     *            new node
     */
    public void addChildren(final TestTreeObject obj) {
        rootLevel.add(obj);
    }

    /**
     * Clear the model
     */
    public void clear() {
        rootLevel.clear();
        pass = 0;
        fail = 0;
        skip = 0;
        cancel = 0;
    }

    /**
     * Search for the right parent for a new node
     * 
     * @param name
     * @return
     */
    public TestTreeObject findNode(final String name) {
        TestTreeObject result;
        for (final TestTreeObject obj : rootLevel) {
            if ((result = obj.findNode(name)) != null) {
                return result;
            }
        }
        return null;
    }

    public void updatePass(final int p) {
        pass += p;
    }

    public int getPass() {
        return pass;
    }

    public void updateFail(final int f) {
        fail += f;
    }

    public int getFail() {
        return fail;
    }

    public void updateSkip(final int s) {
        skip += s;
    }

    public int getSkip() {
        return skip;
    }

    public void updateCancel(final int c) {
        cancel += c;
    }

    public int getCancel() {
        return cancel;
    }

    public void removeChild(final TestTreeObject node) {
        rootLevel.remove(node);
    }

}
