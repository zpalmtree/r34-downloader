import QtQuick 2.6
import QtQuick.Window 2.2
import QtQuick.Layouts 1.3
import QtQuick.Controls 2.2
import QtQuick.Dialogs 1.2

Window {
    property var uiIsEnabled: cancelDisabled

    // Remember to update me on each release!
    title: "R34Downloader - v0.0.10"
    visible: true

    minimumHeight: 350
    minimumWidth: 400

    MessageDialog {
        text: msgText
        title: "Notice"
        visible: msgVisible
        standardButtons: msgButtons
        onRejected: markAsHidden(), cancel()
        onAccepted: markAsHidden()
    }

    GridLayout {
        enabled: uiEnabled
        columns: 2
        anchors.fill: parent
        anchors.margins: 10
        rowSpacing: 10
        columnSpacing: 10

        Label {
            text: "Tag:"
        }
        TextField {
            id: tagInput
            Layout.fillWidth: true
            enabled: uiIsEnabled
        }

        Label {
            text: "Search:"
        }
        Button {
            id: searchButton
            Layout.fillWidth: true
            text: "Search"
            enabled: tagInput.text != "" && uiIsEnabled
            onClicked: search(tagInput.text)
        }

        Label {
            text: "Results:"
        }
        ComboBox {
            id: tagComboBox
            model: searchResults
            Layout.fillWidth: true
            enabled: uiIsEnabled
        }

        Label {
            text: "Directory: "
        }
        Button {
            id: directoryPickerButton
            Layout.fillWidth: true
            text: "Pick a directory"
            onClicked: folderPicker.visible = true
            enabled: uiIsEnabled
        }
        FileDialog {
            id: folderPicker
            selectFolder: true
            onAccepted: directoryPickerButton.text = folder
        }

        Label {
            text: "Download:"
        }
        Button {
            id: downloadButton
            Layout.fillWidth: true
            text: "Download"

            enabled: tagComboBox.currentText != "" &&
                     folderPicker.folder != "" &&
                     uiIsEnabled

            onClicked: download(tagComboBox.currentText, folderPicker.folder)
        }

        Label {
            text: "Cancel:"
        }
        Button {
            id: cancelButton
            Layout.fillWidth: true
            text: "Cancel current operation"
            enabled: !uiIsEnabled
            onClicked: cancel()
        }

        Label {
            text: "Progress:"
        }
        ProgressBar {
            value: progressBar
            Layout.fillWidth: true
        }
    }
}
