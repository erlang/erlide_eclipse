package org.erlide.cover.ui.annotations;

import org.eclipse.jface.text.source.Annotation;

/**
 * Coverage annotations factory
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang.solutions.com>
 * 
 */
public class CoverageAnnotationFactory {

    /**
     * Create new coverage annotation
     * 
     * @param type
     * @return
     */
    public static Annotation create(final String type) {
        return new Annotation(type, true, "");
    }

}
