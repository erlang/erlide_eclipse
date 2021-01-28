package org.erlide.engine.util;

import java.util.Collection;
import java.util.Collections;

import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.xtext.xbase.lib.CollectionLiterals;
import org.erlide.engine.model.root.PathSerializer;
import org.junit.Test;

import com.google.common.truth.Truth;

@SuppressWarnings("all")
public class PathSerializerTest {
    @Test
    public void shouldFilterOutEmptyPaths() {
        final String input = ";a;b;;c;;;d;";
        final Path _path = new Path("a");
        final Path _path_1 = new Path("b");
        final Path _path_2 = new Path("c");
        final Path _path_3 = new Path("d");
        final Collection<IPath> expected = Collections
                .<IPath> unmodifiableList(CollectionLiterals.<IPath> newArrayList(_path,
                        _path_1, _path_2, _path_3));
        final Collection<IPath> actual = PathSerializer.unpackList(input);
        Truth.<IPath, Iterable<IPath>> assertThat(actual).isEqualTo(expected);
    }
}
