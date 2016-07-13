package org.erlide.engine.model.builder;

import static com.google.common.truth.Truth.assertThat;

import java.util.Map;
import java.util.Set;

import org.erlide.engine.model.erlang.MapUtils;
import org.erlide.engine.model.root.ProjectConfigType;
import org.junit.Test;

public class BuilderToolTest {

    @Test
    public void toolsMapMatchesConfigMap() {
        final Map<ProjectConfigType, Set<BuilderTool>> map = ProjectConfigType.configToolsMap;
        final Map<BuilderTool, Set<ProjectConfigType>> inverse = MapUtils.inverseSet(map);
        assertThat(BuilderTool.toolConfigsMap).isEqualTo(inverse);
    }
}
