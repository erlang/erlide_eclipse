package org.erlide.engine.model.erlang;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import java.util.Map;
import java.util.Set;

import org.erlide.engine.model.builder.BuilderConfigType;
import org.erlide.engine.model.builder.BuilderTool;
import org.junit.Test;

public class BuilderConfigTests {

    @Test
    public void configMapMatchesToolMap() {
        final Map<BuilderTool, Set<BuilderConfigType>> map = BuilderTool.toolConfigsMap;
        final Map<BuilderConfigType, Set<BuilderTool>> inverse = MapUtils.inverseSet(map);
        assertThat(BuilderConfigType.configToolsMap, is(inverse));
    }
}
