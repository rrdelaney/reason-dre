const inputEditorEl = document.getElementById('input')
const inputEditor = CodeMirror.fromTextArea(inputEditorEl, {
  lineNumbers: true,
  mode: 'javascript'
})

const outputEditorEl = document.getElementById('output')
const outputEditor = CodeMirror.fromTextArea(outputEditorEl, {
  lineNumbers: true,
  mode: 'rust',
  readOnly: true
})
