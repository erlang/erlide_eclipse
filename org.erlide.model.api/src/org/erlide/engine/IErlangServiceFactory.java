package org.erlide.engine;

import org.erlide.model.root.IErlModel;
import org.erlide.model.services.search.OpenService;
import org.erlide.model.services.search.OtpDocService;
import org.erlide.model.services.search.XrefService;

public interface IErlangServiceFactory {

    IErlModel getModel();

    XrefService getXrefService();

    String getStateDir();

    OpenService getOpenService();

    OtpDocService getOtpDocService();

}
