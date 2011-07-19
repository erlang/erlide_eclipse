package org.erlide.ui;

import org.erlide.core.MessageReporter;
import org.erlide.ui.util.PopupDialog;

public class UIMessageReporter extends MessageReporter {

    public UIMessageReporter() {
    }

    @Override
    public void displayMessage(final MessageType type, final String message,
            final ReporterPosition style) {
        switch (style) {
        case MODAL:
            PopupDialog.showModalDialog("erlide " + type, message);
            break;
        case CENTER:
            PopupDialog.showDialog("erlide " + type, message, 3000);
            break;
        case CORNER:
            PopupDialog.showBalloon("erlide " + type, message, 3000);
            break;
        default:
            break;
        }
    }

}
