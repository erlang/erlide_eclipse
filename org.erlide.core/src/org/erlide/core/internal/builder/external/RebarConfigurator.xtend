package org.erlide.core.internal.builder.external

import org.eclipse.core.resources.IProject
import org.erlide.core.internal.builder.BuilderConfigurator
import org.erlide.engine.model.root.IErlangProjectProperties

class RebarConfigurator implements BuilderConfigurator {

    override encodeConfig(IProject project, IErlangProjectProperties info) {
        '''
            %% coding: utf-8
        '''
    }

    override decodeConfig(String config) {
        throw new UnsupportedOperationException("TODO: auto-generated method stub")
    }
    
    override getConfigFile() {
        'rebar.config'
    }

}
