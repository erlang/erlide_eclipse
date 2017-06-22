package org.erlide.engine.util

import java.util.Collection
import org.eclipse.core.runtime.IPath
import org.eclipse.core.runtime.Path
import org.erlide.engine.model.root.PathSerializer
import org.junit.Test

import static com.google.common.truth.Truth.assertThat

class PathSerializerTest {
    @Test def void shouldFilterOutEmptyPaths() {
        val String input = ";a;b;;c;;;d;"
        val Collection<IPath> expected = #[new Path("a"), new Path("b"),
            new Path("c"), new Path("d")]
        val Collection<IPath> actual = PathSerializer.unpackList(input)
        assertThat(actual).isEqualTo(expected)
    }
}
