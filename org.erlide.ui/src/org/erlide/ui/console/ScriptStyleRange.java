package org.erlide.ui.console;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThanOrEqualTo;
import static org.hamcrest.Matchers.is;

import org.eclipse.swt.custom.StyleRange;
import org.eclipse.swt.graphics.Color;

/**
 * Defines a style range that identifies a given type of content in the console
 * (as defined in the constants)
 *
 * @author Fabio
 */
public class ScriptStyleRange extends StyleRange {

    public static final int UNKNOWN = -1;
    public static final int STDIN = 0;
    public static final int PROMPT = 1;
    public static final int STDOUT = 2;
    public static final int STDERR = 3;

    public int scriptType;

    public ScriptStyleRange(final int start, final int len, final Color fore,
            final Color back, final int scriptType) {
        super(start, len, fore, back);
        assertThat(len, is(greaterThanOrEqualTo(0)));
        this.scriptType = scriptType;
    }

}
