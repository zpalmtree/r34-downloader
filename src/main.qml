import QtQuick 2.6
import QtQuick.Window 2.2
import QtQuick.Layouts 1.3
import QtQuick.Controls 2.2
import QtQuick.Dialogs 1.2

Window {
    title: "Rule34 Downloader"
    visible: true

    minimumHeight: 300
    maximumHeight: 300

    minimumWidth: 400
    maximumWidth: 400

    MessageDialog {
        text: msgText
        title: "Notice"
        visible: msgVisible
        standardButtons: msgButtons
        onRejected: cancel()
    }

    function enableDLButton() {
        return tagComboBox.currentText != "" && folderPicker.folder != ""
    }

    GridLayout {
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
        }

        Label {
            text: "Search:"
        }
        Button {
            id: searchButton
            Layout.fillWidth: true
            text: "Search"
            enabled: tagInput.text != ""
            onClicked: search(tagInput.text)
        }

        Label {
            text: "Results:"
        }
        ComboBox {
            id: tagComboBox
            model: searchResults
            Layout.fillWidth: true
            onCurrentTextChanged: downloadButton.enabled = enableDLButton()
        }

        Label {
            text: "Directory: "
        }
        Button {
            id: directoryPickerButton
            Layout.fillWidth: true
            text: "Pick a directory"
            onClicked: folderPicker.visible = true
            onTextChanged: downloadButton.enabled = enableDLButton()
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
            enabled: false
            onClicked: download(tagComboBox.currentText, folderPicker.folder)
        }
    }
}
