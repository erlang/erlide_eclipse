package org.erlide.engine.model.erlang;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import java.util.Map;
import java.util.Set;

import org.erlide.engine.model.builder.BuilderConfig;
import org.erlide.engine.model.builder.BuilderTool;
import org.junit.Test;

public class BuilderConfigTests {

    @Test
    public void configMapMatchesToolMap() {
        final Map<BuilderTool, Set<BuilderConfig>> map = BuilderTool.toolConfigsMap;
        final Map<BuilderConfig, Set<BuilderTool>> inverse = MapUtils.inverseSet(map);
        assertThat(BuilderConfig.configToolsMap, is(inverse));
    }
}
