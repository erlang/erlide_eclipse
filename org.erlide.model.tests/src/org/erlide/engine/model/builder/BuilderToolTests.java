package org.erlide.engine.model.builder;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import java.util.Map;
import java.util.Set;

import org.erlide.engine.model.erlang.MapUtils;
import org.erlide.engine.model.root.ProjectConfigType;
import org.junit.Test;

public class BuilderToolTests {

    @Test
    public void toolsMapMatchesConfigMap() {
        final Map<ProjectConfigType, Set<BuilderTool>> map = ProjectConfigType.configToolsMap;
        final Map<BuilderTool, Set<ProjectConfigType>> inverse = MapUtils.inverseSet(map);
        assertThat(BuilderTool.toolConfigsMap, is(inverse));
    }
}
