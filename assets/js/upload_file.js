export function upload_file(
  path,
  file,
  onprogress,
  onerror,
  onsuccess
) {
  var xhr = new XMLHttpRequest();
  xhr.onload = onsuccess;
  xhr.onprogress = function (this, ev) {
    onprogress(Math.floor(ev.loaded / ev.total * 100))
  }
  xhr.onerror = onerror
  var form = new FormData()
  form.append("file", file)
  xhr.open("PUT", `${path}/${file.name}`)
  xhr.setRequestHeader("Content-Type", "multipart/form-data")
  xhr.send(form)
}