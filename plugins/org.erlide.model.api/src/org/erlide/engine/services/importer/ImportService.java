package org.erlide.engine.services.importer;

import java.util.List;

public interface ImportService {

    /**
     * Filter import parameters for newly imported erlang project
     *
     * @param prefix
     * @param importSources
     * @param prefix
     *
     * @return
     */
    ErlProjectImport importProject(String prefix, List<String> importSources);

}
