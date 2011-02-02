package org.erlide.cover.core;

/**
 * Interface for marking the code coverage in the editor
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
 * 
 */
public interface ICoverAnnotationMarker {

    /**
     * adds or changes coverage annotations
     */
    public void addAnnotations();

    /**
     * clears coverage annotations
     */
    public void clearAllAnnotations();

}
