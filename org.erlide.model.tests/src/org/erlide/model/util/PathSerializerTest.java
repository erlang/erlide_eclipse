package org.erlide.model.util;

import java.util.Collection;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.erlide.model.root.PathSerializer;
import org.junit.Assert;
import org.junit.Test;

import com.google.common.collect.Lists;

public class PathSerializerTest {

    @Test
    public void shouldFilterOutEmptyPaths() {
        final String input = ";a;b;;c;;;d;";
        final Collection<IPath> expected = Lists.newArrayList((IPath) new Path(
                "a"), (IPath) new Path("b"), (IPath) new Path("c"),
                (IPath) new Path("d"));
        final Collection<IPath> actual = PathSerializer.unpackList(input);
        Assert.assertArrayEquals(expected.toArray(), actual.toArray());
    }
}
