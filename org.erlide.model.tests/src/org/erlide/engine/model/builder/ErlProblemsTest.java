package org.erlide.engine.model.builder;

import org.junit.Test;

public class ErlProblemsTest {

    @Test
    public void loads() {
        final ErlProblems p = ErlProblems.instance;
        p.check();
    }
}
