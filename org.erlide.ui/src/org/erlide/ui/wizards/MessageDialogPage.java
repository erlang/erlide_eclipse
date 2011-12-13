/*******************************************************************************
 * Copyright (c) 2006 Vlad Dumitrescu and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at 
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     Vlad Dumitrescu
 *******************************************************************************/
package org.erlide.ui.wizards;

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.DialogPage;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.IMessageProvider;
import org.eclipse.jface.resource.JFaceColors;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

class MessageDialogPage extends DialogPage {

    static class MessageRegion {

        private Text messageText;

        private Label messageImageLabel;

        private Composite messageComposite;

        private String lastMessageText = "";//$NON-NLS-1$

        private int lastMessageType;

        /**
         * Create a new instance of the receiver.
         */
        public MessageRegion() {
            // No initial behavior
        }

        /**
         * Create the contents for the receiver.
         * 
         * @param parent
         *            the Composite that the children will be created in
         */
        public void createContents(final Composite parent) {
            messageComposite = new Composite(parent, SWT.NONE);
            final GridLayout messageLayout = new GridLayout();
            messageLayout.numColumns = 2;
            messageLayout.marginWidth = 0;
            messageLayout.marginHeight = 0;
            messageLayout.makeColumnsEqualWidth = false;
            messageComposite.setLayout(messageLayout);
            messageImageLabel = new Label(messageComposite, SWT.NONE);

            final GridData imageData = new GridData(
                    GridData.VERTICAL_ALIGN_CENTER);
            final Image sizingImage = JFaceResources
                    .getImage(Dialog.DLG_IMG_MESSAGE_ERROR);
            Rectangle imageBounds;
            if (sizingImage == null) {
                imageBounds = new Rectangle(0, 0,
                        IDialogConstants.VERTICAL_MARGIN * 2,
                        IDialogConstants.VERTICAL_MARGIN * 2);
            } else {
                imageBounds = sizingImage.getBounds();
            }
            imageData.heightHint = imageBounds.height
                    + IDialogConstants.VERTICAL_SPACING;
            imageData.widthHint = imageBounds.width
                    + IDialogConstants.HORIZONTAL_SPACING;
            messageImageLabel.setLayoutData(imageData);

            messageText = new Text(messageComposite, SWT.NONE);
            messageText.setEditable(false);
            messageText.setBackground(parent.getDisplay().getSystemColor(
                    SWT.COLOR_WIDGET_BACKGROUND));

            final GridData textData = new GridData(GridData.GRAB_HORIZONTAL
                    | GridData.FILL_HORIZONTAL | GridData.VERTICAL_ALIGN_CENTER);
            messageText.setLayoutData(textData);
            hideRegion();

        }

        /**
         * Set the layoutData for the messageArea. In most cases this will be a
         * copy of the layoutData used in setTitleLayoutData.
         * 
         * @param layoutData
         *            the layoutData for the message area composite.
         */
        public void setMessageLayoutData(final Object layoutData) {
            messageComposite.setLayoutData(layoutData);
        }

        /**
         * Show the new message in the message text and update the image. Base
         * the background color on whether or not there are errors.
         * 
         * @param newMessage
         *            The new value for the message
         * @param newType
         *            One of the IMessageProvider constants. If newType is
         *            IMessageProvider.NONE show the title.
         * @see IMessageProvider
         */
        public void updateText(final String newMessage, final int newType) {
            Image newImage = null;
            boolean showingError = false;
            switch (newType) {
            case IMessageProvider.NONE:
                hideRegion();
                return;
            case IMessageProvider.INFORMATION:
                newImage = JFaceResources.getImage(Dialog.DLG_IMG_MESSAGE_INFO);
                break;
            case IMessageProvider.WARNING:
                newImage = JFaceResources
                        .getImage(Dialog.DLG_IMG_MESSAGE_WARNING);
                break;
            case IMessageProvider.ERROR:
                newImage = JFaceResources
                        .getImage(Dialog.DLG_IMG_MESSAGE_ERROR);
                showingError = true;
                break;
            }

            if (newMessage == null) {// No message so clear the area
                hideRegion();
                return;
            }
            showRegion();
            // Any more updates required
            if (newMessage.equals(messageText.getText())
                    && newImage == messageImageLabel.getImage()) {
                return;
            }
            messageImageLabel.setImage(newImage);
            messageText.setText(newMessage);
            if (showingError) {
                setMessageColors(JFaceColors
                        .getErrorBackground(messageComposite.getDisplay()));
            } else {
                lastMessageText = newMessage;
                setMessageColors(JFaceColors
                        .getBannerBackground(messageComposite.getDisplay()));
            }

        }

        /**
         * Show and enable the widgets in the message region
         */
        private void showRegion() {
            messageComposite.setVisible(true);
        }

        /**
         * Hide the message region and clear out the caches.
         */
        private void hideRegion() {
            messageComposite.setVisible(false);
            lastMessageText = null;
            lastMessageType = IMessageProvider.NONE;
        }

        /**
         * Set the colors of the message area.
         * 
         * @param color
         *            The color to be use in the message area.
         */
        private void setMessageColors(final Color color) {
            messageText.setBackground(color);
            messageComposite.setBackground(color);
            messageImageLabel.setBackground(color);
        }

        /**
         * Clear the error message. Restore the previously displayed message if
         * there is one, if not restore the title label.
         * 
         */
        public void clearErrorMessage() {
            updateText(lastMessageText, lastMessageType);
        }
    }

    MessageRegion fMessageRegion;

    public MessageDialogPage(final Composite parent) {
        createControl(parent);
    }

    @Override
    public void createControl(final Composite parent) {
        final Composite composite1 = new Composite(parent, SWT.NONE);
        final GridLayout layout = new GridLayout();
        layout.marginWidth = 0;
        layout.marginHeight = 0;
        composite1.setLayout(layout);
        fMessageRegion = new MessageRegion();
        fMessageRegion.createContents(composite1);
        final GridData messageData = new GridData(GridData.FILL_HORIZONTAL
                | GridData.GRAB_HORIZONTAL);
        fMessageRegion.setMessageLayoutData(messageData);
        setControl(composite1);
    }

    @Override
    public void setMessage(final String newMessage, final int newType) {
        super.setMessage(newMessage, newType);
        fMessageRegion.updateText(newMessage, newType);
    }

    @Override
    public void setErrorMessage(final String newMessage) {
        super.setErrorMessage(newMessage);
        fMessageRegion.updateText(newMessage, IMessageProvider.ERROR);
    }

}
