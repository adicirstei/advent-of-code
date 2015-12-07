var c = require('crypto')

module.exports = function (input) {
  var i = 0, res = '', md5
  do {
    md5 = c.createHash('md5')
    md5.update(input + i)
    i++
    res = md5.digest('hex')
  } while (! /^0{6}/.test(res))
  return i-1
}
