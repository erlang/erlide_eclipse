package org.erlide.engine.util;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import java.util.Collection;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.erlide.engine.model.root.PathSerializer;
import org.junit.Test;

import com.google.common.collect.Lists;

public class PathSerializerTest {

    @Test
    public void shouldFilterOutEmptyPaths() {
        final String input = ";a;b;;c;;;d;";
        final Collection<IPath> expected = Lists.newArrayList((IPath) new Path("a"),
                (IPath) new Path("b"), (IPath) new Path("c"), (IPath) new Path("d"));
        final Collection<IPath> actual = PathSerializer.unpackList(input);
        assertThat(actual.toArray(), is(expected.toArray()));
    }
}
