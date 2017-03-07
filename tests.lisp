(in-package :ichiran/test)

(defmacro assert-segment (str &rest segmentation)
  `(assert-equal ',segmentation
                 (mapcar (lambda (wi) (if (eql (word-info-type wi) :gap) :gap
                                          (word-info-text wi)))
                         (simple-segment ,str))
                 ))

(define-test segmentation-test
  (assert-segment "ご注文はうさぎですか" "ご注文" "は" "うさぎ" "です" "か")
  (assert-segment "しませんか" "しません" "か")
  (assert-segment "ドンマイ" "ドンマイ")
  (assert-segment "みんな土足でおいで" "みんな" "土足で" "おいで")
  (assert-segment "おもわぬオチ提供中" "おもわぬ" "オチ" "提供" "中")
  (assert-segment "わたし" "わたし")
  (assert-segment "お姉ちゃんにまかせて地球まるごと"
                  "お姉ちゃん" "に" "まかせて" "地球" "まるごと")
  (assert-segment "大人になってるはず"
                  "大人" "に" "なってる" "はず")
  (assert-segment "いいとこ" "いいとこ")
  (assert-segment "そういうお隣どうし"
                  "そういう" "お" "隣" "どうし")
  (assert-segment "はしゃいじゃう" "はしゃいじゃう")
  (assert-segment "分かっちゃうのよ" "分かっちゃう" "の" "よ")
  (assert-segment "懐かしく新しいまだそしてまた"
                  "懐かしく" "新しい" "まだ" "そして" "また")
  (assert-segment "あたりまえみたいに思い出いっぱい"
                  "あたりまえ" "みたい" "に" "思い出" "いっぱい")
  (assert-segment "何でもない日々とっておきのメモリアル"
                  "何でもない" "日々" "とっておき" "の" "メモリアル")
  (assert-segment "しつれいしなければならないんです"
                  "しつれいし" "なければならない" "ん" "です")
  (assert-segment "だけど気付けば馴染んじゃってる"
                  "だけど" "気付けば" "馴染んじゃってる")
  (assert-segment "飲んで笑っちゃえば"
                  "飲んで" "笑っちゃえば")
  (assert-segment "なんで" "なんで")
  (assert-segment "遠慮しないでね" "遠慮しないで" "ね")
  (assert-segment "出かけるまえに" "出かける" "まえ" "に")
  (assert-segment "感じたいでしょ" "感じたい" "でしょ")
  (assert-segment "まじで" "まじ" "で")
  (assert-segment "その山を越えたとき" "その" "山" "を" "越えた" "とき")
  (assert-segment "遊びたいのに" "遊びたい" "のに")
  (assert-segment "しながき" "しながき")
  (assert-segment "楽しさ求めて" "楽しさ" "求めて")
  (assert-segment "日常のなかにも" "日常" "の" "なかにも")
  (assert-segment "ほんとは好きなんだと" "ほんと" "は" "好き" "な" "ん" "だと")
  (assert-segment "内緒なの" "内緒" "なの")
  (assert-segment "魚が好きじゃない" "魚" "が" "好き" "じゃない")
  (assert-segment "物語になってく" "物語" "に" "なってく")
  (assert-segment "書いてきてくださった" "書いてきて" "くださった")
  (assert-segment "今日は何の日" "今日" "は" "何の" "日")
  (assert-segment "何から話そうか" "何から" "話そう" "か")
  (assert-segment "話したくなる" "話したく" "なる")
  (assert-segment "進化してく友情" "進化してく" "友情")
  (assert-segment "私に任せてくれ" "私" "に" "任せてくれ")
  (assert-segment "時までに帰ってくると約束してくれるのなら外出してよろしい"
                  "時" "まで" "に" "帰ってくる" "と"
                  "約束してくれる" "の" "なら" "外出して" "よろしい")
  (assert-segment "雨が降りそうな気がします" "雨" "が" "降りそう" "な" "気がします")
  (assert-segment "新しそうだ" "新しそう" "だ")
  (assert-segment "本を読んだりテレビを見たりします"
                  "本" "を" "読んだり" "テレビ" "を" "見たり" "します")
  (assert-segment "今日母はたぶんうちにいるでしょう"
                  "今日" "母" "は" "たぶん" "うち" "に" "いる" "でしょう")
  (assert-segment "赤かったろうです" "赤かったろう" "です")
  (assert-segment "そう呼んでくれていい" "そう" "呼んでくれていい")
  (assert-segment "払わなくてもいい" "払わなくてもいい")
  (assert-segment "体に悪いと知りながらタバコをやめることはできない"
                  "体" "に" "悪い" "と" "知り" "ながら" "タバコをやめる" "こと" "は" "できない")
  (assert-segment "いつもどうり" "いつも" "どうり")
  (assert-segment "微笑みはまぶしすぎる" "微笑み" "は" "まぶしすぎる")
  (assert-segment "なにをしていますか" "なに" "を" "しています" "か")
  (assert-segment "優しすぎそのうえカッコいいの" "優しすぎ" "そのうえ" "カッコいい" "の")
  (assert-segment "この本は複雑すぎるから" "この" "本" "は" "複雑" "すぎる" "から")
  (assert-segment "かわいいです" "かわいいです")
  (assert-segment "学生なんだ" "学生" "な" "ん" "だ")
  (assert-segment "なんだから" "な" "ん" "だから")
  (assert-segment "名付けたい" "名付けたい")
  (assert-segment "切なくなってしまう" "切なく" "なってしまう")
  (assert-segment "らいかな" "らい" "かな")
  (assert-segment "誰かいなくなった" "誰か" "いなくなった")
  (assert-segment "思い出すな" "思い出す" "な")
  (assert-segment "かなって思ったら" "かなって" "思ったら")
  (assert-segment "ことすら難しい" "こと" "すら" "難しい")
  (assert-segment "投下しました" "投下しました")
  (assert-segment "車止める" "車" "止める")
  (assert-segment "円盤はただの" "円盤" "は" "ただ" "の")
  (assert-segment "なんですね" "な" "ん" "です" "ね")
  (assert-segment "ズボンからすねをむき出しにする"
                  "ズボン" "から" "すね" "を" "むき" "出しにする")
  (assert-segment "駅の前で会いましょう"
                  "駅" "の" "前" "で" "会いましょう")
  (assert-segment "あなたの質問は答えにくい"
                  "あなた" "の" "質問" "は" "答えにくい")
  (assert-segment "とかそういう" "とか" "そういう")
  (assert-segment "好評のうちに" "好評" "の" "うち" "に")
  (assert-segment "映像もすごくよかったです" "映像" "も" "すごく" "よかったです")
  (assert-segment "情けねえ" "情けねえ")
  (assert-segment "春ですねえ" "春" "です" "ねえ")
  (assert-segment "春ですねぇ" "春" "です" "ねぇ")
  (assert-segment "きつねじゃなかった" "きつね" "じゃなかった")
  (assert-segment "ワシじゃなくて和紙じゃよ" "ワシ" "じゃなくて" "和紙" "じゃ" "よ")
  (assert-segment "ほうがいいよ" "ほうがいい" "よ")
  (assert-segment "痛さはどれくらいですか" "痛さ" "は" "どれくらい" "です" "か")
  (assert-segment "見てくれたかな" "見てくれた" "かな")
  (assert-segment "とても良かった" "とても" "良かった")
  (assert-segment "戻りたいかと言われる" "戻りたい" "か" "と" "言われる") ;;should be と言われる at some point
  (assert-segment "こういうのでいいんだよ" "こういう" "の" "でいい" "ん" "だ" "よ")
  (assert-segment "そんなのでいいと思ってるの" "そんな" "の" "でいい" "と" "思ってる" "の")
  (assert-segment "だけが墓参りしてた" "だけ" "が" "墓参りしてた")
  (assert-segment "はいいんだけどな" "は" "いい" "ん" "だけど" "な")
  (assert-segment "なりつつあるんだが" "なりつつある" "ん" "だが")
  (assert-segment "反論は認めません" "反論" "は" "認めません")
  (assert-segment "見たような気がする" "見た" "ような" "気がする")
  (assert-segment "元気になる" "元気" "に" "なる")
  (assert-segment "半端なかった" "半端なかった")
  (assert-segment "一人ですね" "一人" "です" "ね")
  (assert-segment "行事がある" "行事" "が" "ある")
  (assert-segment "当てられたものになる" "当てられた" "ものになる")
  (assert-segment "獲得しうる" "獲得しうる")
  (assert-segment "ことができず" "ことができず")
  (assert-segment "一生一度だけの忘られぬ約束" "一生一度" "だけ" "の" "忘られぬ" "約束")
  (assert-segment "やらずにこの路線でよかったのに" "やらず" "に" "この" "路線" "で" "よかった" "のに")
  (assert-segment "歌ってしまいそう" "歌ってしまいそう")
  (assert-segment "しまいそう" "しまいそう")
  (assert-segment "まいそう祭り" "まいそう" "祭り")
  (assert-segment "何ですか" "何" "です" "か")
  (assert-segment "浮かれたいから" "浮かれたい" "から")
  (assert-segment "なくなっちゃう" "なくなっちゃう")
  (assert-segment "になりそうだけど" "に" "なりそう" "だけど")
  (assert-segment "これは辛い選択になりそうだな" "これ" "は" "辛い" "選択" "に" "なりそう" "だ" "な")
  (assert-segment "はっきりしそうだな" "はっきりしそう" "だ" "な")
  (assert-segment "泣きそうなんだけど" "泣きそう" "な" "ん" "だけど")
  (assert-segment "これですね" "これ" "です" "ね")
  (assert-segment "はいなくなります" "は" "いなく" "なります")
  (assert-segment "忘れなく" "忘れなく")
  (assert-segment "じゃないですか" "じゃないです" "か")
  (assert-segment "純粋さ健気さ" "純粋さ" "健気さ")
  (assert-segment "着てたからね" "着てた" "から" "ね")
  (assert-segment "仕出かすからだと思います" "仕出かす" "から" "だと" "思います")
  (assert-segment "みんながした" "みんな" "が" "した")
  (assert-segment "ほうが速いと" "ほう" "が" "速い" "と")
  (assert-segment "注意してください" "注意してください")
  (assert-segment "昨日といいどうしてこう" "昨日" "と" "いい" "どうして" "こう")
  (assert-segment "いっぱいきそう" "いっぱい" "きそう")
  (assert-segment "仲良しになったら" "仲良し" "に" "なったら")
  (assert-segment "全くといっていい" "全く" "と" "いって" "いい")
  (assert-segment "発狂しそうなんだ" "発狂しそう" "な" "ん" "だ")
  (assert-segment "していたんだ" "していた" "ん" "だ")
  (assert-segment "引き上げられた" "引き上げられた")
  (assert-segment "をつかむため" "を" "つかむ" "ため")
  (assert-segment "ときが自分" "とき" "が" "自分")
  (assert-segment "もうこころ" "もう" "こころ")
  (assert-segment "届けしたら" "届け" "したら")
  (assert-segment "おまえら低いんだよ" "おまえら" "低い" "ん" "だ" "よ")
  (assert-segment "すべてがかかっていると思いながら" "すべて" "が" "かかっている" "と" "思い" "ながら")
  ;;(assert-segment "がいないとこの" "が" "いない" "と" "この")
  (assert-segment "エロいと思っちゃう" "エロい" "と" "思っちゃう")
  (assert-segment "変わり映えしない" "変わり映えしない")
  (assert-segment "あなたがいなきゃこんな計画思いつかなかった"
                  "あなた" "が" "いなきゃ" "こんな" "計画" "思いつかなかった")
  (assert-segment "見たかったです" "見たかったです")
  (assert-segment "出来て楽しかったな" "出来て" "楽しかった" "な")
  (assert-segment "つかってください" "つかってください")
  (assert-segment "誰もが思ってた" "誰も" "が" "思ってた")
  (assert-segment "参考にしたらしい" "参考にしたらしい")
  (assert-segment "狙いやすそうで" "狙い" "やすそう" "で")
  (assert-segment "予定はございませんので" "予定" "は" "ございません" "ので")
  (assert-segment "名目上ではね" "名目上" "で" "は" "ね")
  (assert-segment "犬はトラックにはねられた" "犬" "は" "トラック" "に" "はねられた")
  (assert-segment "仕事してください" "仕事してください")
  (assert-segment "おいかけっこしましょ" "おい" "かけっこしましょ")
  (assert-segment "イラストカードが付きます" "イラスト" "カード" "が" "付きます")
  (assert-segment "じゃないかしら" "じゃない" "かしら")
  (assert-segment "いつか本当に" "いつか" "本当に")
  (assert-segment "言い方もします" "言い方" "も" "します")
  (assert-segment "何でこれ" "何で" "これ")
  (assert-segment "こういう物語ができるんだ" "こういう" "物語" "が" "できる" "ん" "だ")
  (assert-segment "といったところでしょうか" "といった" "ところ" "でしょうか")
  (assert-segment "広めたいと思っている" "広めたい" "と" "思っている")
  (assert-segment "のせいかな" "の" "せい" "かな")
  (assert-segment "その場合" "その" "場合")
  (assert-segment "教えてくれてありがとう" "教えてくれて" "ありがとう")
  (assert-segment "彼が来るかどうか疑問だ" "彼" "が" "来る" "かどうか" "疑問" "だ")
  (assert-segment "泳ぎに行ってはどうかな" "泳ぎ" "に" "行って" "は" "どうかな")
  (assert-segment "どうか僕を許して下さい" "どうか" "僕" "を" "許して" "下さい")
  (assert-segment "鏡はいらないですよ" "鏡" "は" "いらないです" "よ")
  (assert-segment "ベッドで跳ねちゃいけません" "ベッド" "で" "跳ねちゃ" "いけません")
  (assert-segment "お酒を飲んじゃだめです" "お酒" "を" "飲んじゃ" "だめ" "です")
  (assert-segment "これ洗濯しといて" "これ" "洗濯しといて")
  (assert-segment "来週までに読んどいて" "来週" "まで" "に" "読んどいて")
  (assert-segment "奴がまともに見られない" "奴" "が" "まとも" "に" "見られない")
  (assert-segment "間違いなし" "間違いなし")
  (assert-segment "見ませんでしょうか" "見ません" "でしょうか")
  (assert-segment "書いていただけませんでしょうか" "書いて" "いただけません" "でしょうか")
  (assert-segment "友達できる" "友達" "できる")
  (assert-segment "実はそうなんだ" "実は" "そう" "なんだ")
  (assert-segment "やらしいです" "やらしいです")
  (assert-segment "荒いとこもある" "荒い" "とこ" "も" "ある")
  (assert-segment "あったかいとこ行こう" "あったかい" "とこ" "行こう")
  (assert-segment "ぶっちゃけ話" "ぶっちゃけ" "話")
  (assert-segment "いけないわー" "いけない" "わ" :gap)
  (assert-segment "社長としてやっていけないわ" "社長" "として" "やっていけない" "わ")
  (assert-segment "よくわかんないけど" "よく" "わかんない" "けど")
  (assert-segment "ほうがいいんじゃないの" "ほうがいい" "ん" "じゃない" "の")
  (assert-segment "こんなんじゃ" "こんな" "ん" "じゃ")
  (assert-segment "増やしたほうがいいな" "増やした" "ほうがいい" "な")
  (assert-segment "屈しやすいものだ" "屈し" "やすい" "もの" "だ")
  (assert-segment "目をもっている" "目" "を" "もっている")
  (assert-segment "これが君のなすべきものだ" "これ" "が" "君" "の" "なす" "べき" "もの" "だ")
  (assert-segment "泥棒をつかまえた" "泥棒" "を" "つかまえた")
  (assert-segment "金もないし友達もいません" "金" "も" "ない" "し" "友達" "も" "いません")
  (assert-segment "出来たからほら見てよ" "出来た" "から" "ほら" "見て" "よ")
  (assert-segment "眠いからもう寝るね" "眠い" "から" "もう" "寝る" "ね")
  (assert-segment "浮気してやがった" "浮気してやがった")
  (assert-segment "見本通りに" "見本" "通り" "に")
  (assert-segment "不適応" "不" "適応")
  (assert-segment "良いそうです" "良い" "そう" "です")
  (assert-segment "むらむらとわいた" "むらむら" "と" "わいた")
  (assert-segment "否定しちゃいけない" "否定しちゃ" "いけない")
  (assert-segment "観たいです" "観たいです")
  (assert-segment "あんたはわからん" "あんた" "は" "わからん")
  (assert-segment "見られたくないとこ" "見られたくない" "とこ")
  (assert-segment "多分家で" "多分" "家" "で")
  (assert-segment "三十八" "三十" "八")
  (assert-segment "エロそうだヤバそうだ" "エロそう" "だ" "ヤバそう" "だ")
  (assert-segment "私にとっても" "私" "にとって" "も")
  (assert-segment "睡眠を十分にとってください" "睡眠" "を" "十分" "に" "とってください")
  (assert-segment "そうなんだけど" "そう" "な" "ん" "だけど")
  (assert-segment "進んでない" "進んでない")
  (assert-segment "一回だけであとは言わない" "一回" "だけ" "で" "あと" "は" "言わない")
  (assert-segment "ご親切に恐縮しております" "ご親切に" "恐縮しております")
  (assert-segment "官吏となっておる者がある" "官吏" "と" "なっておる" "者" "が" "ある")
  (assert-segment "間違えておられたようですね" "間違えておられた" "ようです" "ね")
  (assert-segment "人気のせいな" "人気" "の" "せい" "な")
  (assert-segment "コレはアレ" "コレ" "は" "アレ")
  (assert-segment "アレハレ" :gap) 
  (assert-segment "上に文字があったり" "上" "に" "文字" "が" "あったり")
  (assert-segment "言っただろ" "言った" "だろ")
  (assert-segment "嵐が起ころうとしている" "嵐" "が" "起ころうとしている")
  (assert-segment "知らないでしょう" "知らないでしょう")
  (assert-segment "読まないでしょう" "読まないでしょう")
  (assert-segment "来ないでしょう" "来ないでしょう")
  (assert-segment "何もかもがめんどい" "何もかも" "が" "めんどい")
  (assert-segment "なにもかもがめんどい" "なにもかも" "が" "めんどい")
  (assert-segment "あいつ規制されりゃいいのに" "あいつ" "規制されりゃ" "いい" "のに")
  (assert-segment "塗ってみようと思って" "塗って" "みよう" "と" "思って")
  (assert-segment "肩を並べられなかった" "肩を並べられなかった")
  (assert-segment "じゃなくて良かった" "じゃなくて" "良かった")
  (assert-segment "申し訳なさそう" "申し訳なさそう")
  (assert-segment "決まってたし" "決まってた" "し")
  (assert-segment "決まっている" "決まっている")
  (assert-segment "恐れ入りました" "恐れ入りました")
  (assert-segment "はうまい" "は" "うまい")
  (assert-segment "弾け飛びました" "弾け飛びました")
  (assert-segment "ぶっこんでいるようで" "ぶっこんでいる" "よう" "で")
  (assert-segment "じゃないけど下手に" "じゃない" "けど" "下手" "に")
  (assert-segment "的にそうではない" "的" "に" "そう" "ではない")
  (assert-segment "入り込めなかった" "入り込めなかった")
  (assert-segment "がいまいちなんだよ" "が" "いまいち" "な" "ん" "だ" "よ")
  (assert-segment "脱がしにかかってる" "脱がし" "に" "かかってる")
  (assert-segment "必死になってる" "必死" "に" "なってる")
  (assert-segment "安心させた" "安心させた")
  (assert-segment "人が好きそうだ" "人" "が" "好き" "そう" "だ")
  (assert-segment "もっていこうとする" "もっていこうとする")
  (assert-segment "増やして" "増やして")
  (assert-segment "ぜいたくで" "ぜいたく" "で")
  (assert-segment "したくらいで" "したくらい" "で")
  (assert-segment "でもうまく人" "でも" "うまく" "人")
  (assert-segment "好き嫌いもしないように" "好き嫌い" "も" "しない" "ように")
  (assert-segment "のどこが思える" "の" "どこ" "が" "思える")
  (assert-segment "出会えて良かった" "出会えて" "良かった")
  (assert-segment "無理しなくていいから" "無理しなくていい" "から")
  (assert-segment "調子にのらないほうが" "調子" "に" "のらない" "ほう" "が")
  (assert-segment "こなさそう" "こなさそう")
  (assert-segment "伸びてこなさそう" "伸びてこなさそう")
  (assert-segment "手にとって" "手にとって")
  (assert-segment "平和である" "平和" "で" "ある")
  (assert-segment "私にとっては少しおかしいです" "私" "にとって" "は" "少し" "おかしいです")
  (assert-segment "パーティーは" "パーティー" "は")
  (assert-segment "彼以上のばかはいない" "彼" "以上" "の" "ばか" "は" "いない")
  (assert-segment "君がいないと淋しい" "君" "が" "いない" "と" "淋しい")
  (assert-segment "思いきって" "思いきって")
  (assert-segment "思いきっている" "思いきっている")
  (assert-segment "大事にします" "大事" "に" "します")
  (assert-segment "大事になります" "大事" "に" "なります")
  (assert-segment "ご迷惑おかけしてすみません" "ご迷惑" "おかけして" "すみません")
  (assert-segment "不便をおかけすることを謝ります" "不便" "を" "おかけする" "こと" "を" "謝ります")
  (assert-segment "お手数おかけし申し訳ないが" "お手数" "おかけし" "申し訳ない" "が")
  (assert-segment "私はあなたにお手数をおかけました" "私" "は" "あなた" "に" "お手数" "を" "お" "かけました")
  (assert-segment "ここにおかけなさい" "ここ" "に" "お" "かけなさい")
  (assert-segment "弾き出されてる" "弾き出されてる")
  )

(define-test json-consistency-test
  (loop for word in '("の" "赤かったろう" "書いてきてる" "捩じり鉢巻きで"
                      "夕べ" "さくや" "建ち並ばなきゃ" "建ち並びましてる" 
                      "どおりで"
                      )
       for word-info = (word-info-from-text word)
       for word-info-json = (word-info-json word-info)
       do (assert-equal (word-info-gloss-json word-info) (word-info-gloss-json word-info-json))))


(define-test match-readings-test
  (assert-equal (match-readings "取次店" "とりつぎてん") '(("取" "とり" "ja_kun" NIL) ("次" "つぎ" "ja_kun" NIL) ("店" "てん" "ja_on" NIL)))
  (assert-equal (match-readings "助っ人" "すけっと") '(("助" "すけ" "ja_kun" NIL) "っ" ("人" "と" "ja_kun" NIL)))
  (assert-equal (match-readings "頑張って" "がんばって") '(("頑" "がん" "ja_on" NIL) ("張" "ば" "ja_kun" NIL) "って"))
  (assert-equal (match-readings "肝心" "かんじん") '(("肝" "かん" "ja_on" NIL) ("心" "じん" "ja_on" :RENDAKU NIL)))
  (assert-equal (match-readings "決心" "けっしん") '(("決" "けっ" "ja_on" NIL "つ") ("心" "しん" "ja_on" NIL)))
  (assert-equal (match-readings "行方" "ゆくえ") '(("行" "ゆ" "ja_kun" NIL) ("方" "くえ" "irr")))
  (assert-equal (match-readings "時計" "とけい") '(("時" "と" "irr") ("計" "けい" "ja_on" NIL)))
  (assert-equal (match-readings "指図" "さしず") '(("指" "さし" "irr") ("図" "ず" "ja_on" NIL)))
  (assert-equal (match-readings "竹刀" "しない") '(("竹" "しな" "irr") ("刀" "い" "irr")))
  (assert-equal (match-readings "小売店" "こうりてん") '(("小" "こ" "ja_kun" NIL) ("売" "うり" "irr") ("店" "てん" "ja_on" NIL)))
  (assert-equal (match-readings "売上げ" "うりあげ") '(("売" "うり" "irr") ("上" "あ" "ja_kun" NIL) "げ"))
  (assert-equal (match-readings "北方" "ほっぽう") '(("北" "ほっ" "ja_on" NIL "く") ("方" "ぽう" "ja_on" :RENDAKU NIL)))
  (assert-equal (match-readings "明後日" "あさって") '(("明" "あ" "ja_kun" NIL) ("後" "さっ" "irr") ("日" "て" "irr")))
  )
  
;; (lisp-unit:run-tests '(ichiran/test::match-readings-test) :ichiran/test)

(defun run-all-tests ()
  (init-suffixes t)
  (let ((res (run-tests :all :ichiran/test)))
    (print-failures res)
    (print-errors res)
    res))
