package org.erlide.engine.model.builder;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.contains;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.nullValue;

import java.util.List;

import org.junit.Test;

public class ErlProblemsTest {

    @Test
    public void loads() {
        final ErlProblems p = ErlProblems.getInstance();
        p.check();
    }

    @Test
    public void messageArgs1() {
        final ProblemData data = ErlProblems.parse("circular macro 'alfa-beta'");
        assertThat(data, is(not(nullValue())));
        final List<String> args = data.getMessageArgs("circular macro 'alfa-beta'");
        assertThat(args, contains("alfa-beta"));
    }

    @Test
    public void messageArgs2() {
        final ProblemData data = ErlProblems.parse("circular macro 'alfa/beta'");
        assertThat(data, is(not(nullValue())));
        final List<String> args = data.getMessageArgs("circular macro 'alfa/beta'");
        assertThat(args, contains("alfa", "beta"));
    }

    @Test
    public void properlyOrderedTags() {
        for (final ProblemData data : ErlProblems.getInstance().getData()) {
            final ProblemData data2 = ErlProblems.parse(data.getMessage());
            assertThat(data2.getTag(), is(data.getTag()));
            assertThat(data2.getMessage(), is(data.getMessage()));
            assertThat(data2.getArity(), is(data.getArity()));
        }
    }

    @Test
    public void arity() {
        final int n = ErlProblems.arity("~ ~ \\~ ~");
        assertThat(n, is(3));
    }

    @Test
    public void keepTildes() {
        final ProblemData data = new ProblemData("t", "~ \\~ ~", 2);
        assertThat(data.getPattern().toString(), is("(.+?) ~ (.+?)"));
    }
}
