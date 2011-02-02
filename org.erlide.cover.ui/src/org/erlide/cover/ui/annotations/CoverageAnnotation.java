package org.erlide.cover.ui.annotations;

import org.eclipse.jface.text.source.Annotation;

/**
 * Coverage annotations factory
 * 
 * @author Aleksandra Lipiec <aleksandra.lipiec@erlang.solutions.com>
 *
 */
public class CoverageAnnotation {

    public static final String FULL_COVERAGE = 
        "org.erlide.cover.ui.fullCoverageAnnotation";
    public static final String NO_COVERAGE =
        "org.erlide.cover.ui.noCoverageAnnotation"; 
    
    public static Annotation create(String type) {
        return new Annotation(type, true, "");
    }
    
}
