package org.erlide.engine.model.builder;

import static com.google.common.truth.Truth.assertThat;

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
        assertThat(data).isNotNull();
        final List<String> args = data.getMessageArgs("circular macro 'alfa-beta'");
        assertThat(args).contains("alfa-beta");
    }

    @Test
    public void messageArgs2() {
        final ProblemData data = ErlProblems.parse("circular macro 'alfa/beta'");
        assertThat(data).isNotNull();
        final List<String> args = data.getMessageArgs("circular macro 'alfa/beta'");
        assertThat(args).containsAllOf("alfa", "beta");
    }

    @Test
    public void properlyOrderedTags() {
        for (final ProblemData data : ErlProblems.getInstance().getData()) {
            final ProblemData data2 = ErlProblems.parse(data.getMessage());
            assertThat(data2.getTag()).isEqualTo(data.getTag());
            assertThat(data2.getMessage()).isEqualTo(data.getMessage());
            assertThat(data2.getArity()).isEqualTo(data.getArity());
        }
    }

    @Test
    public void arity() {
        final int n = ErlProblems.arity("~ ~ \\~ ~");
        assertThat(n).isEqualTo(3);
    }

    @Test
    public void keepTildes() {
        final ProblemData data = new ProblemData("t", "~ \\~ ~", 2);
        assertThat(data.getPattern().toString()).isEqualTo("(.+?) ~ (.+?)");
    }
}
