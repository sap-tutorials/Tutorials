
var expect = require('chai').expect;
var checklink = require('./');

describe('checklink index.js', function() {

  it('result.isPassed should equal true when url is taobao.com', function(done) {

    checklink('https://taobao.com')
      .then(function(result) {
        expect(result.isPassed).to.equal(true);
        done();
      });

  });

  it('result.isPassed should equal false when url is xxxhhhhhh.com', function(done) {

    checklink('http://xxxhhhhhh.com')
      .then(function(result) {
        expect(result.isPassed).to.equal(false);
        done();
      });

  });

  it('result.deadlinks.length should equal 2 when urls is array and have two deadlinks', function(done) {

    checklink(['http://baidu.com', 'https://taobao.com', 'xxxxxxxx', 'hxhxhhxhx.com.cn'])
      .then(function(result) {

        console.log(result);

        expect(result.isPassed).to.equal(false);
        expect(result.deadlinks.length).to.equal(2);
        done();
      });

  });

});
