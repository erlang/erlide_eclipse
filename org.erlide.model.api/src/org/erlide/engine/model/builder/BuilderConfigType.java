package org.erlide.engine.model.builder;

import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.Set;

import org.eclipse.xtext.xbase.lib.Functions;

import com.google.common.collect.Maps;
import com.google.common.collect.Sets;

public enum BuilderConfigType {
    INTERNAL("org.erlide.core"), EMAKE("Emakefile"), REBAR("rebar.config");

    private final String configName;

    BuilderConfigType(final String configName) {
        this.configName = configName;
    }

    /**
     * A string that points to where the configuration is stored for this
     * project. The configurator interprets the value as fit, it can be a file
     * name or a preference node name.
     */
    public String getConfigName() {
        return configName;
    }

    public static final Map<BuilderConfigType, Set<BuilderTool>> configToolsMap = new Functions.Function0<Map<BuilderConfigType, Set<BuilderTool>>>() {
        @Override
        public Map<BuilderConfigType, Set<BuilderTool>> apply() {
            final Map<BuilderConfigType, Set<BuilderTool>> result = Maps.newHashMap();
            result.put(INTERNAL, Sets.newHashSet(BuilderTool.INTERNAL, BuilderTool.MAKE));
            result.put(EMAKE, Sets.newHashSet(BuilderTool.EMAKE, BuilderTool.MAKE,
                    BuilderTool.INTERNAL));
            result.put(REBAR, Sets.newHashSet(BuilderTool.REBAR, BuilderTool.MAKE,
                    BuilderTool.INTERNAL));
            return Maps.newEnumMap(result);
        }
    }.apply();

    /**
     * @return the list of BuilderTools that can be used with this configurator
     */
    public Collection<BuilderTool> getMatchingTools() {
        return Collections.unmodifiableCollection(configToolsMap.get(this));
    }

    public boolean matchTool(final BuilderTool tool) {
        return configToolsMap.get(this).contains(tool);
    }
}
