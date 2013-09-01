package org.erlide.engine;

import org.erlide.model.root.IErlModel;
import org.erlide.model.services.search.XrefService;

public interface IErlangServiceFactory {

    IErlModel getModel();

    XrefService getXrefService();

    String getStateDir();

}
