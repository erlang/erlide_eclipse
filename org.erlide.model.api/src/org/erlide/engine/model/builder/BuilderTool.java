package org.erlide.engine.model.builder;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.eclipse.xtext.xbase.lib.Functions;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

public enum BuilderTool {
    INTERNAL(null), MAKE("Makefile"), EMAKE("Emakefile"), REBAR("rebar.config");

    private final String toolMarker;

    BuilderTool(final String toolMarker) {
        this.toolMarker = toolMarker;
    }

    /**
     * The marker is a top-level file in the project that tells us to use this
     * tool.
     */
    public String getToolMarker() {
        return toolMarker;
    }

    private static Map<BuilderTool, List<BuilderConfig>> toolConfigsMap = new Functions.Function0<Map<BuilderTool, List<BuilderConfig>>>() {
        @Override
        public Map<BuilderTool, List<BuilderConfig>> apply() {
            final Map<BuilderTool, List<BuilderConfig>> result = Maps.newHashMap();
            result.put(INTERNAL, Lists.newArrayList(BuilderConfig.INTERNAL));
            result.put(MAKE, Lists.newArrayList(BuilderConfig.INTERNAL,
                    BuilderConfig.EMAKE, BuilderConfig.REBAR));
            result.put(EMAKE, Lists.newArrayList(BuilderConfig.EMAKE));
            result.put(REBAR, Lists.newArrayList(BuilderConfig.REBAR));
            return Maps.newEnumMap(result);
        }
    }.apply();

    public Collection<BuilderConfig> getMatchingConfigs() {
        return Collections.unmodifiableCollection(toolConfigsMap.get(this));
    }

}
