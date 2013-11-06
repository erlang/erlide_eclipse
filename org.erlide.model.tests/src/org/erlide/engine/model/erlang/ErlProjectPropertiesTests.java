package org.erlide.engine.model.erlang;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import org.erlide.engine.model.root.ErlangProjectProperties;
import org.junit.Test;

public class ErlProjectPropertiesTests {

    @Test
    public void equalsTest() {
        final ErlangProjectProperties p1 = new ErlangProjectProperties();
        final ErlangProjectProperties p2 = new ErlangProjectProperties();
        assertThat(p2, is(ErlangProjectPropertiesMatcher.sameAs(p1)));
    }

}
