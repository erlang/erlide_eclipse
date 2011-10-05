package org.erlide.cover.ui.annotations;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.eclipse.jface.text.source.Annotation;
import org.erlide.cover.views.model.LineResult;

/**
 * Stores information about which lines are marked with coverage annotations
 * 
 * @author Aleksnadra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 * 
 */
public class CoverageMap {

    private final Map<String, Map<LineResult, Annotation>> coverage;

    public CoverageMap() {
        coverage = new HashMap<String, Map<LineResult, Annotation>>();
    }

    /**
     * Adds annotation for specified file at specified line.
     * 
     * @param name
     * @param lr
     * @param ann
     */
    public void addAnnotation(final String name, final LineResult lr,
            final Annotation ann) {
        if (!coverage.containsKey(name)) {
            coverage.put(name, new HashMap<LineResult, Annotation>());
        }

        coverage.get(name).put(lr, ann);
    }

    /**
     * Gets annotation from specified file in specified line
     * 
     * @param name
     * @param lr
     * @return
     */
    public Annotation getAnnotation(final String name, final LineResult lr) {
        if (!coverage.containsKey(name)) {
            return null;
        }
        return coverage.get(name).get(lr);
    }

    /**
     * Check if an there is any coverage annotation at specified line
     * 
     * @param name
     * @param lr
     * @return
     */
    public boolean containsAnnotation(final String name, final LineResult lr) {
        return coverage.containsKey(name) && coverage.get(name).containsKey(lr)
                && coverage.get(name).get(lr) != null;
    }

    public boolean containsFile(final String name) {
        return coverage.containsKey(name);
    }

    /**
     * Check the type of specified cverage annotation
     * 
     * @param name
     * @param lr
     * @param type
     * @return
     */
    public boolean checkType(final String name, final LineResult lr,
            final String type) {
        return containsAnnotation(name, lr)
                && coverage.get(name).get(lr).getType().equals(type);
    }

    /**
     * Remove coverage annotations.
     * 
     * @param name
     * @param lr
     */
    public void removeAnnotation(final String name, final LineResult lr) {
        if (containsAnnotation(name, lr)) {
            coverage.get(name).remove(lr);
        }
    }

    /**
     * Remove all annotations for specified file
     * 
     * @param name
     */
    public void removeAll(final String name) {
        coverage.get(name).clear();
    }

    public void removeAll() {
        coverage.clear();
    }

    public Set<LineResult> getLineSet(final String name) {
        return new HashSet<LineResult>(coverage.get(name).keySet());
    }

}
