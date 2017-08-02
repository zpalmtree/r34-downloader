import QtQuick 2.6
import QtQuick.Window 2.2
import QtQuick.Layouts 1.3
import QtQuick.Controls 2.2
import QtQuick.Dialogs 1.2

Window {
    visible: true
    title: "Rule34 Downloader"
    minimumHeight: 300
    minimumWidth: 400

    MessageDialog {
        text: msgText
        title: "Notice"
        visible: msgVisible
        standardButtons: msgButtons
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
            onTextChanged: searchButton.enabled = text != ""
        }

        Label {
            text: "Search:"
        }
        Button {
            id: searchButton
            Layout.fillWidth: true
            text: "Search"
            enabled: false
            onClicked: search(tagInput.text)
        }

        Label {
            text: "Results:"
        }
        ComboBox {
            model: searchResults
            Layout.fillWidth: true
        }

        Label {
            text: "Directory: "
        }
        Button {
            id: directoryPickerButton
            Layout.fillWidth: true
            text: "Pick a directory"
            onClicked: folderPicker.visible = true
        }
        FileDialog {
            id: folderPicker
            selectFolder: true

            onAccepted: directoryPickerButton.text = folderPicker.folder
        }

        Label {
            text: "Download:"
        }
        Button {
            id: downloadButton
            Layout.fillWidth: true
            text: "Download"
            enabled: false
        }
    }
}
