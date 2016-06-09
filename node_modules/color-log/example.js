var log = require('./color-log.js');
var test_object = {test: 'object', number: 1};
console.log("without");
log.info('you read this', test_object);
log.mark('you read this better', test_object);
log.warn('we require more vespene gas', test_object);
log.error('houston we have a problem', test_object);

var slow = 'slow';
slowpoke_say();
function slowpoke_say()
{
    slow = slow.replace('o', 'oo');
    log.single.info(test_object, "slowpoke say:", slow);
    setTimeout(slowpoke_say, 500);
}