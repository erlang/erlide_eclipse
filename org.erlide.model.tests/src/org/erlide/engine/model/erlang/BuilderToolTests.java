package org.erlide.engine.model.erlang;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import java.util.Map;
import java.util.Set;

import org.erlide.engine.model.builder.BuilderConfig;
import org.erlide.engine.model.builder.BuilderTool;
import org.junit.Test;

public class BuilderToolTests {

    @Test
    public void toolsMapMatchesConfigMap() {
        final Map<BuilderConfig, Set<BuilderTool>> map = BuilderConfig.configToolsMap;
        final Map<BuilderTool, Set<BuilderConfig>> inverse = MapUtils.inverseSet(map);
        assertThat(BuilderTool.toolConfigsMap, is(inverse));
    }
}
