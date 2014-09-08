package org.erlide.engine.model.erlang;

import org.erlide.engine.model.root.ErlangProjectProperties;
import org.hamcrest.BaseMatcher;
import org.hamcrest.Description;
import org.hamcrest.Factory;
import org.hamcrest.Matcher;

public class ErlangProjectPropertiesMatcher extends BaseMatcher<ErlangProjectProperties> {

    private final ErlangProjectProperties item;

    public ErlangProjectPropertiesMatcher(final ErlangProjectProperties item) {
        this.item = item;
    }

    @Override
    public boolean matches(final Object other) {
        return item.equals(other);
    }

    @Override
    public void describeTo(final Description description) {
        description.appendText("is same as ").appendText(item.toString());
    }

    @Factory
    public static <T> Matcher<ErlangProjectProperties> sameAs(
            final ErlangProjectProperties item) {
        return new ErlangProjectPropertiesMatcher(item);
    }

}
