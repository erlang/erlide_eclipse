package org.erlide.cover.ui.annotations;

import org.eclipse.jface.text.source.Annotation;

public class CoverageAnnotation extends Annotation {

    public static final String FULL_COVERAGE = 
        "org.erlide.cover.ui.fullCoverageAnnotation";
    public static final String NO_COVERAGE =
        "org.erlide.cover.ui.noCoverageAnnotation"; //TODO
    
    public CoverageAnnotation(String type) {
        super(type, true, "");
    }
    
}
