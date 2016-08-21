# Back-end API specification

## End user API

For the user who check the coupon.

* No authentication is required for any API
* No user specific information is saved on back-end side
    * Any such data are saved on front-end with localStorage.

### GET recommended coupons

GET recommended coupons data for current user settings.

#### Sample request and response

Though the behavior of this API implies that it's better to use GET method, it might be good to use POST method to send this complex request with full JSON expression.

```bash
$ curl -G "http://$domain/api/v1/coupon" \
  --data-urlencode 'areas=[{"latitude": 41.40338, "longitude": 2.17403}]' \
  --data-urlencode 'tags=[4,6,8,9]' \
  --data-urlencode 'category=1' \
  --data-urlencode 'favorites=[2,50,230]' \
  --data-urlencode 'history=[430,2,220,50,22,300,230]' \
  | jq '.'

{
  "next": null,
  "coupons": [
    {
      "id": 3,
      "store": {
        "id": 52,
        "category": 1,
        "tags": [3, 4, 8],
        "image": "http://s3.amasonaws.com/foo/baz",
        "name": "七輪焼き肉・安安",
        "description": "七輪焼き肉・安安は、美味しい焼肉を...",
        "address": "渋谷区桜丘町2-12 渋谷亀八ビル4F",
        "area": {"latitude": 41.40243, "longitude": 2.17551},
        "phoneNumber": "03-3464-0722",
        "hours": "月〜木 17:00〜翌4:30\n金土日祝・祝前 16:00〜翌4:30",
        "closeOn": "元旦のみ",
        "url": "http://www.fuji-tatsu.co.jp/",
      },
      "image": "http://s3.amasonaws.com/foo/bar",
      "title": "当日OK! 21時以降のご予約で2.5H飲放題付き料理4品で3,600円",
      "validFrom": "Sun, 15 May 2016 00:00:00 GMT",
      "expire": "Mon, 30 May 2016 00:00:00 GMT",
      "type": "set",
      "setContent": "2.5時間飲放題付きの料理4品です。\n1. 前菜\n2. 焼き肉盛り合わせ\n 3. 冷麺\n 4. アイスクリーム",
      "setPrice": 3600,
      "setReferencePrice": 4000,
      "setOtherConditions": "※ 1グループ1回のご利用時の利用枚数制限はありません。\n※ 21時以降のご予約のお客様が対象です。"
    },
    ...
  ]
}
```

#### Request Parameter and its type

```haskell
-- Dummy data type representing request parameter from front-end
data RequestParam = RequestParam
  {
    -- Only coupons whose location is near by the any area
    -- specified by this parameter
    areas :: [Area]
    -- Tag IDs that the user chose as he/she is interested in
  , tags  :: [Tag]
    -- Category ID of coupons to show
  , category :: Category
    -- Coupon IDs that the user put it into favorite box
    -- The left side is the coupon ID that the user put it into for the last time
  , favorites :: [CouponId]
    -- Coupon IDs that the user has ever seen details
    -- For practical, the front-end will cut off only N newest history
    -- The left side is the coupon ID that the user saw for the last time
  , history :: [CouponId]
    -- This parameter is for infinite loading
    -- The response generate resulting list of coupons, and drops while this Coupon appears
    -- i.e., The first coupon id of response is this value
    -- If this value is `Nothing`, DO NOT drop coupons
  , next :: Maybe CouponId
    -- Response only returns first N of resulting lists,
    -- where N is the value specified by this
    -- If not specified, first 100 (default value) coupons are returned
  , limit :: Maybe Int
  }

-- ==============
--  Helper Types
-- ==============

data Area = Area
  { latitude :: Latitude
  , longitude :: Longitude
  }
newtype Latitude = Latitude { unLatitude :: Double }
newtype Longitude = Longitude { unLongitude :: Double }
newtype Tag = Tag { unTag :: Int }
newtype Category = Category { unCategory :: Int }
newtype CouponId = CouponId { unCouponId :: Int }
```

#### Response and its type

This code is for explanation of the API response, so this is NOT the same structure as production code.

```haskell
-- Dummy data type representing response from back-end
data Response = Response
  {
    -- Next cursor of this search parameters
    -- i.e., To implement infinite scroll,
    --       the front-end send next request
    --       by setting this value on `next` parameter of request
    next :: Maybe CouponId
    -- Coupons matching request search parameters
  , coupons :: [Coupon]
  }

data Coupon = Coupon
  {
    -- An ID of this coupon
    couponId :: CouponId
    -- Information about the store that provide this coupon
  , couponStore :: Store
    -- An image for illustration purposes of the coupon
    -- If the image does not present, this returns default image
  , couponImage :: Image
    -- A title of this coupon
  , couponTitle :: Maybe Text
    -- The coupon can be used from this date
    -- Also, the coupon cannot be seen by end-user before this date
  , couponValidFrom :: Maybe UTCDate
    -- Expire date of this coupon
    -- After this date, the coupon does not appear in kucipong search result
    -- The store can reject the coupon, if an end-user show this coupon after the expire date
  , couponExpire :: Maybe UTCDate
    -- Coupon type
  , couponType :: CouponType

    -- ======== For `couponType` is `CouponDiscount` ========
    -- Percentage style discount rate about total fee
    -- i.e., (Actual fee) = (Total fee) * (100 - discountRate) `div` 100
  , discountRate :: Maybe PercentInt
    -- Minimum total fee to use this coupon
  , discountMinimumFee :: Maybe Int
    -- Other conditions to use this coupon
  , discountOtherConditions :: Maybe Text

    -- ======== For `couponType` is `CouponGift` ========
    -- Description about the gift
  , giftContent :: Maybe Text
    -- Reference price of the gift
  , giftReferencePrice :: Maybe Int
    -- Minimum total fee to use this coupon
  , giftMinimumFee :: Maybe Int
    -- Other conditions to use this coupon
  , giftOtherConditions :: Maybe Text

    -- ======== For `couponType` is `CouponSet` ========
    -- Description about the  set
  , setContent :: Maybe Text
    -- The  price of the set
  , setPrice :: Maybe Int
    -- Reference price of the  set
    -- i.e., The total fee to buy the same items without this coupon
  , setReferencePrice :: Maybe Int
    -- Other conditions to use this coupon
  , setOtherConditions :: Maybe Text

    -- ======== For `couponType` is `CouponOther` ========
    -- Desctiption about this coupon
  , otherContent :: Maybe Text
    -- Conditions to use this coupon
  , otherConditions :: Maybe Text
  }

data Store = Store
  {
    -- An ID of this store
    storeId :: StoreId
    -- Store name
  , storeName :: Maybe Text
    -- Store category
  , storeCategory :: Maybe Category
    -- Tags symbolizing the store
  , storeTags :: [Tag]
    -- Main image illustrating the store
  , storeImage :: Maybe Image
    -- Store description to show end-users
  , storeDescription :: Maybe Text
    -- Store address
  , storeAddress :: Maybe Text
    -- Latitude / Longitude calculated from 'storeAddress'
  , storeAddress :: Maybe Area
    -- Phone number end-users can contact store staffs
  , storePhoneNumber :: Maybe Text
    -- What time is the store open?
  , storeHours :: Maybe Text
    -- What day is it the store closed?
  , storeCloseOn :: Maybe Text
    -- Official URL of the store
  , storeURL :: Maybe URL
  }

data CouponType =
    -- A coupon to discount total amount of fee
    CouponDiscount
    -- A coupon to get free gifts if buy something
  | CouponGift
    -- A coupon to buy special set with special price
  | CouponSet
    -- A coupon of other format
  | CouponOther

-- ==============
--  Helper Types
-- ==============

newtype Image = Image { unImage :: Text }
newtype PercentInt = PercentInt { unPercentInt :: Int }
```

### GET coupon detail

GET detail information of a coupon in server side rendered HTML format.

#### Sample request and response

This response is a sample simple HTML for convenience.
Some user may bookmark or share this URI and search engine also crawl this page, so do not include version number in the URI.
I suggest the back-end Haskell program should read independent template file (e.g, `/static/coupon.html`) when compilation, which makes more maintainability for us.

```bash
$ curl -G "http://$domain/coupon/${coupon_id}"

<!DOCTYPE html>
<html lang="ja">
  <head>
    <meta content="IE=edge" http-equiv="X-UA-Compatible">
    <meta content="text/html; charset=UTF-8" http-equiv="Content-Type">
    <meta content="width=device-width,initial-scale=1" name="viewport">
    <title>当日OK! 21時以降のご予約で2.5H飲放題付き料理4品で3,600円</title>
    <link rel="stylesheet" href="/static/main.css">
  </head>
  <body>
    <div class="coupon">
      <div class="card">
        <div class="card_header">
          <h2 class="card_header_text">七輪焼肉・安安</h2>
          <div class="card_header_icon"><span class="icon-like" data-coupon-id="3"></span></div>
        </div>
        <div class="card_image">
          <img src="http://s3.amasonaws.com/foo/bar" alt="七輪焼き肉・安安">
        </div>
        <div class="card_body">
          <div class="card_body_title">
            当日OK! 21時以降のご予約で2.5H飲放題付き料理4品で3,600円
          </div>
          <div class="card_body_summary">
            <span class="card_body_summary-sub">10% OFF</span>
            <span class="card_body_summary-main">3600円</span>
          </div>
          <div class="card_body_expiration">
            <span class="card_body_expiration-title">有効期限</span>
            <span class="card_body_expiration-body">2016年4月15日から 2016年4月30日まで</span>
          </div>
        </div>
      </div>
      <div class="annotation">※ この画面を店舗でご提示ください</div>
      <div class="location">
        <div class="location_map" data-latitude="41.40243" data-longitude="2.17551"></div>
      </div>
      <div class="aboutStore">
        <a class="btn outerBtn" href="/store/52">七輪焼き肉・安安</a>
      </div>
      <div class="moreContents">
        <p>2.5時間飲放題付きの料理4品です。1. 前菜 2. 焼き肉盛り合わせ 3. 冷麺 4. アイスクリーム"</p>
        <p>※ 1グループ1回のご利用時の利用枚数制限はありません。※ 21時以降のご予約のお客様が対象です。</p>
      </div>
    </div>
  </body>
</html>
```

#### Request Parameter and its type

```haskell
couponId :: CouponId
```

#### Response template

This code is for explanation of the API response, so this is NOT the same HTML as production code.

* Model

    ```haskell
coupon :: Coupon  -- Data representing the coupon of given ID
    ```
* Pseudo template file

    ```html
<!DOCTYPE html>
<html lang="ja">
  <head>
    <meta content="IE=edge" http-equiv="X-UA-Compatible">
    <meta content="text/html; charset=UTF-8" http-equiv="Content-Type">
    <meta content="width=device-width,initial-scale=1" name="viewport">
    <title>#{couponTitle coupon}</title>
    <link rel="stylesheet" href="/static/main.css">
  </head>
  <body>
    <div class="coupon">
      <div class="card">
        <div class="card_header">
          <h2 class="card_header_text">#{storeName (couponStore coupon)}</h2>
          <div class="card_header_icon"><span class="icon-like" data-coupon-id="#{couponId coupon}"></span></div>
        </div>
        <div class="card_image">
          <img src="#{couponImage coupon}" alt="#{storeName (couponStore coupon)}">
        </div>
        <div class="card_body">
          <div class="card_body_title">
            #{couponTitle coupon}
          </div>

          <div class="card_body_summary">
            #{{ if (couponType == CouponDiscount) then }}
            <span class="card_body_summary-sub">#{discountMinimumFee coupon}円以上のお買い上げで</span>
            <span class="card_body_summary-main">#{discountRate coupon}% OFF</span>

            #{{ elseif (couponType == CouponGift) then }}
            <span class="card_body_summary-sub">#{giftMinimumFee coupon}円以上のお買い上げで</span>
            <span class="card_body_summary-main">#{maybe "非売品" ((<> "円相当の品") . tshow) (giftReferencePrice coupon)} をプレゼント</span>

            #{{ elseif (couponType == CouponSet) then }}
            <span class="card_body_summary-sub">#{100 - (setPrice coupon * 100 `div` setReferencePrice coupon)}% OFF</span>
            <span class="card_body_summary-main">#{setPrice coupon}円</span>

            #{{ elseif (couponType == CouponOther) then }}
            <span class="card_body_summary-main">#{otherContent coupon}</span>

            #{{ endif }}
          </div>


          <div class="card_body_expiration">
            <span class="card_body_expiration-title">有効期限</span>
            <span class="card_body_expiration-body">#{formatDateJa (couponValidFrom coupon)}から #{formatDateJa (couponExpire coupon)}まで</span>
          </div>
        </div>
      </div>
      <div class="annotation">※ この画面を店舗でご提示ください</div>
      <div class="location">
        <div class="location_map" data-latitude="#{(latitude . storeArea . couponStore) coupon}" data-longitude="#{(longitude . storeArea . couponStore) coupon"></div>
      </div>
      <div class="aboutStore">
        <a class="btn outerBtn" href="/store/#{storeId (couponStore coupon)}">#{storeName (couponStore coupon)}</a>
      </div>
      <div class="moreContents">

        #{{ if (couponType == CouponDiscount) then }}
        <p>#{discountMinimumFee coupon}円以上のお買い上げのお客様が対象です。</p>
        <p>#{discountOtherConditions coupon}</p>

        #{{ elseif (couponType == CouponGift) then }}
        <p>#{giftMinimumFee coupon}円以上のお買い上げのお客様を対象に #{giftContent coupon} をプレゼントいたします。</p>
        <p>#{giftOtherConditions coupon}</p>

        #{{ elseif (couponType == CouponSet) then }}
        <p>クーポン限定セット #{setContent coupon} をお買い上げいただけます。</p>
        <p>#{setOtherConditions coupon}</p>

        #{{ elseif (couponType == CouponOther) then }}
        <p>#{otherContent coupon}</p>
        <p>#{otherConditions coupon}</p>
        #{{ endif }}

      </div>
    </div>
  </body>
</html>
```

### GET store detail

GET detail information of a store in server side rendered HTML format.

#### Sample request and response

This response is a sample simple HTML for convenience.
Some user may bookmark or share this URI and search engine also crawl this page, so do not include version number in the URI.

```bash
$ curl -G "http://$domain/store/${store_id}"

<!DOCTYPE html>
<html lang="ja">
  <head>
    <meta content="IE=edge" http-equiv="X-UA-Compatible">
    <meta content="text/html; charset=UTF-8" http-equiv="Content-Type">
    <meta content="width=device-width,initial-scale=1" name="viewport">
    <title>七輪焼肉・安安</title>
    <link rel="stylesheet" href="/static/main.css">
  </head>
  <body>
    <header class="storeHeader">
      <div class="storeHeader_image_wrapper">
        <img src="http://s3.amasonaws.com/foo/baz" alt="七輪焼肉・安安">
        <span class="storeHeader_annotation">画像は一例です</span>
      </div>
      <h2 class="storeHeader_title">七輪焼肉・安安</h2>
    </header>
    <div class="storeBody">
      <div class="storeBody_description">
        <p>七輪焼き肉・安安は、美味しい焼肉を...</p>
        <p>...</p>
      </div>
      <div class="storeBody_viewCoupon">
        <a class="btn defaultBtn" href="http://$domain/coupon/3">クーポンを見る</a>
      </div>
      <div class="storeBody_info card">
        <div class="storeBody_info_title">店舗情報</div>
        <div class="storeBody_info_body card_body">
          <div class="card_row">
            <div class="card_row_header">
              住所
            </div>
            <div class="card_row_body">
              渋谷区桜丘町2-12 渋谷亀八ビル4F
            </div>
          </div>
          <div class="card_row">
            <div class="card_row_header">
              電話番号
            </div>
            <div class="card_row_body">
              03-3464-0722
            </div>
          </div>
          <div class="card_row">
            <div class="card_row_header">
              営業時間
            </div>
            <div class="card_row_body">
              月〜木 17:00〜翌4:30\n金土日祝・祝前 16:00〜翌4:30
            </div>
          <div class="card_row">
            <div class="card_row_header">
              定休日
            </div>
            <div class="card_row_body">
              元旦のみ
            </div>
          </div>
          <div class="card_row">
            <a class="btn outerBtn" href="http://www.fuji-tatsu.co.jp/">オフィシャルサイト</a>
          </div>
        </div>
      </div>
    </div>
  </body>
</html>
```

#### Request Parameter and its type

```haskell
storeId :: StoreId
```

#### Response template

This code is for explanation of the API response, so this is NOT the same HTML as production code.

* Model

    ```haskell
store :: Store  -- Data representing the store of given ID
    ```
* Pseudo template file

    ```html
<!DOCTYPE html>
<html lang="ja">
  <head>
    <meta content="IE=edge" http-equiv="X-UA-Compatible">
    <meta content="text/html; charset=UTF-8" http-equiv="Content-Type">
    <meta content="width=device-width,initial-scale=1" name="viewport">
    <title>#{storeName store}</title>
    <link rel="stylesheet" href="/static/main.css">
  </head>
  <body>
    <header class="storeHeader">
      <div class="storeHeader_image_wrapper">
        <img src="#{storeImage store}" alt="#{storeName store}">
        <span class="storeHeader_annotation">画像は一例です</span>
      </div>
      <h2 class="storeHeader_title">#{storeName store}</h2>
    </header>
    <div class="storeBody">
      <div class="storeBody_description">
        #{{ withParagraphTag (storeDescription store) }}
      </div>
      <div class="storeBody_viewCoupon">
        <a class="btn defaultBtn" href="http://$domain/store/#{storeId store}/coupon">クーポンを見る</a>
      </div>
      <div class="storeBody_info card">
        <div class="storeBody_info_title">店舗情報</div>
        <div class="storeBody_info_body card_body">
          <div class="card_row">
            <div class="card_row_header">
              住所
            </div>
            <div class="card_row_body">
              #{storeAddress store}
            </div>
          </div>
          <div class="card_row">
            <div class="card_row_header">
              電話番号
            </div>
            <div class="card_row_body">
              #{storePhoneNumber store}
            </div>
          </div>
          <div class="card_row">
            <div class="card_row_header">
              営業時間
            </div>
            <div class="card_row_body">
              #{storeHours store}
            </div>
          </div>
          <div class="card_row">
            <div class="card_row_header">
              定休日
            </div>
            <div class="card_row_body">
              #{storeCloseOn store}
            </div>
          </div>
          <div class="card_row">
            <a class="btn outerBtn" href="#{storeURL store}">オフィシャルサイト</a>
          </div>
        </div>
      </div>
    </div>
  </body>
</html>
```

### GET coupons of a store

GET detail information of a store in server side rendered HTML format.

#### Sample request and response

This response is a sample simple HTML for convenience.
Some user may bookmark or share this URI and search engine also crawl this page, so do not include version number in the URI.

```bash
$ curl -G "http://$domain/store/${store_id}/coupon"

<!DOCTYPE html>
<html lang="ja">
  <head>
    <meta content="IE=edge" http-equiv="X-UA-Compatible">
    <meta content="text/html; charset=UTF-8" http-equiv="Content-Type">
    <meta content="width=device-width,initial-scale=1" name="viewport">
    <title>七輪焼肉・安安</title>
    <link rel="stylesheet" href="/static/main.css">
  </head>
  <body>
    <div class="coupon">
      <div class="card">
        <div class="card_header">
          <h2 class="card_header_text">七輪焼肉・安安</h2>
          <div class="card_header_icon"><span class="icon-like" data-coupon-id="3"></span></div>
        </div>
        <div class="card_image">
          <img src="http://s3.amasonaws.com/foo/bar" alt="七輪焼き肉・安安">
        </div>
        <div class="card_body">
          <div class="card_body_title">
            当日OK! 21時以降のご予約で2.5H飲放題付き料理4品で3,600円
          </div>
          <div class="card_body_summary">
            <span class="card_body_summary-sub">10% OFF</span>
            <span class="card_body_summary-main">3600円</span>
          </div>
          <div class="card_body_expiration">
            <span class="card_body_expiration-title">有効期限</span>
            <span class="card_body_expiration-body">2016年4月15日から 2016年4月30日まで</span>
          </div>
        </div>
      </div>
    </div>
    <div class="coupon">
      <div class="card">
        <div class="card_header">
          <h2 class="card_header_text">七輪焼肉・安安</h2>
          <div class="card_header_icon"><span class="icon-like" data-coupon-id="3"></span></div>
        </div>
        <div class="card_image">
          <img src="http://s3.amasonaws.com/foo/bar" alt="七輪焼き肉・安安">
        </div>
        <div class="card_body">
          <div class="card_body_title">
            当日OK! 21時以降のご予約で2.5H飲放題付き料理4品で3,600円
          </div>
          <div class="card_body_summary">
            <span class="card_body_summary-sub">10% OFF</span>
            <span class="card_body_summary-main">3600円</span>
          </div>
          <div class="card_body_expiration">
            <span class="card_body_expiration-title">有効期限</span>
            <span class="card_body_expiration-body">2016年4月15日から 2016年4月30日まで</span>
          </div>
        </div>
      </div>
    </div>
    <div class="coupon">
      <div class="card">
        <div class="card_header">
          <h2 class="card_header_text">七輪焼肉・安安</h2>
          <div class="card_header_icon"><span class="icon-like" data-coupon-id="3"></span></div>
        </div>
        <div class="card_image">
          <img src="http://s3.amasonaws.com/foo/bar" alt="七輪焼き肉・安安">
        </div>
        <div class="card_body">
          <div class="card_body_title">
            当日OK! 21時以降のご予約で2.5H飲放題付き料理4品で3,600円
          </div>
          <div class="card_body_summary">
            <span class="card_body_summary-sub">10% OFF</span>
            <span class="card_body_summary-main">3600円</span>
          </div>
          <div class="card_body_expiration">
            <span class="card_body_expiration-title">有効期限</span>
            <span class="card_body_expiration-body">2016年4月15日から 2016年4月30日まで</span>
          </div>
        </div>
      </div>
    </div>
  </body>
</html>
```

#### Request Parameter and its type

```haskell
storeId :: StoreId
```

#### Response template

This code is for explanation of the API response, so this is NOT the same HTML as production code.

* Model

    ```haskell
store :: Store  -- Data representing the store of given ID
coupons :: [Coupon] -- Data representing coupons associated to the store of given ID
    ```
* Pseudo template file

    ```html
<!DOCTYPE html>
<html lang="ja">
  <head>
    <meta content="IE=edge" http-equiv="X-UA-Compatible">
    <meta content="text/html; charset=UTF-8" http-equiv="Content-Type">
    <meta content="width=device-width,initial-scale=1" name="viewport">
    <title>#{storeName store}</title>
    <link rel="stylesheet" href="/static/main.css">
  </head>
  <body>

    {{ for coupon in coupons }}
    <div class="coupon">
      <div class="card">
        <div class="card_header">
          <h2 class="card_header_text">#{storeName (couponStore coupon)}</h2>
          <div class="card_header_icon"><span class="icon-like" data-coupon-id="#{couponId coupon}"></span></div>
        </div>
        <div class="card_image">
          <img src="#{couponImage coupon}" alt="#{storeName (couponStore coupon)}">
        </div>
        <div class="card_body">
          <div class="card_body_title">
            #{couponTitle coupon}
          </div>

          <div class="card_body_summary">
            #{{ if (couponType == CouponDiscount) then }}
            <span class="card_body_summary-sub">#{discountMinimumFee coupon}円以上のお買い上げで</span>
            <span class="card_body_summary-main">#{discountRate coupon}% OFF</span>

            #{{ elseif (couponType == CouponGift) then }}
            <span class="card_body_summary-sub">#{giftMinimumFee coupon}円以上のお買い上げで</span>
            <span class="card_body_summary-main">#{maybe "非売品" ((<> "円相当の品") . tshow) (giftReferencePrice coupon)} をプレゼント</span>

            #{{ elseif (couponType == CouponSet) then }}
            <span class="card_body_summary-sub">#{100 - (setPrice coupon * 100 `div` setReferencePrice coupon)}% OFF</span>
            <span class="card_body_summary-main">#{setPrice coupon}円</span>

            #{{ elseif (couponType == CouponOther) then }}
            <span class="card_body_summary-main">#{otherContent coupon}</span>

            #{{ endif }}
          </div>


          <div class="card_body_expiration">
            <span class="card_body_expiration-title">有効期限</span>
            <span class="card_body_expiration-body">#{formatDateJa (couponValidFrom coupon)}から #{formatDateJa (couponExpire coupon)}まで</span>
          </div>
        </div>
      </div>
    </div>
    {{ endfor }}
  </body>
</html>
```

## Store user API

For the user who supply the coupon.

* No password is required to log in
    * Always send special log in page URI to the registered email
* No way to register store user by themselves
    * Only way to register is done by admin user
* Authorization token is needed on any API request

### Request user verification

#### Sample request and response

Note that an email with verification URL will be sent to the email, too.

```bash
$ curl -X POST "http://$domain/api/store/v1/request-verification" \
  --data-urlencode 'email=example@example.com' \
  | jq '.'

{
  "result": "Email was successfully sent."
}
```

#### Request Parameter and its type

```haskell
email : Email
newtype Email = Email { unEmail :: Text }
```

#### Response and its type

```haskell
-- Dummy data type representing response from back-end
data Response = Response
  { result :: Maybe Text
  }
```

### User verification

#### Sample request and response

This URI is contained in email sent by the "Request user verification" API.

```bash
$ curl -G "http://$domain/api/store/v1/verification/${one_time_verification_key}"

(Redirect to the store home page)
```

#### Request Parameter and its type

```haskell
oneTimeVerificationKey :: VerKey
newtype VerKey = VerKey { unVerKey :: Text }
```

#### Response and its type

Return nothing but some side effects are occured, if the verification code is valid.
(Note that the verification code should live only one hour)

* Redirect to the store home page
    * `/store`
* Set token for authorization on HTTP header

### GET my store information

#### Sample request and response

This response is a sample simple HTML for convenience.
Some user may bookmark or share this URI and search engine also crawl this page, so do not include version number in the URI.

```bash
$ curl -G "http://$domain/store" \
  -H "AUTH-TOKEN: ${token}"

<!DOCTYPE html>
<html lang="ja">
  <head>
    <meta content="IE=edge" http-equiv="X-UA-Compatible">
    <meta content="text/html; charset=UTF-8" http-equiv="Content-Type">
    <meta content="width=device-width,initial-scale=1" name="viewport">
    <title>kucipong ストア管理</title>
    <link rel="stylesheet" href="/static/main.css">
  </head>
  <body>
    <div class="storeMenu">
      <a class="btn defaultBtn" href="/store/edit">ストア情報編集</a>
      <a class="btn outerBtn" href="/store/coupon/create">クーポン一覧</a>
    </div>
    <div class="storeBody card">
      <div class="storeBody_info_body card_body">
        <div class="card_row">
          <div class="card_row_header">
            店舗名
          </div>
          <div class="card_row_body">
            七輪焼肉・安安
          </div>
        </div>
        <div class="card_row">
          <div class="card_row_header">
            カテゴリ
          </div>
          <div class="card_row_body">
            グルメ
          </div>
        </div>
        <div class="card_row">
          <div class="card_row_header">
            店舗の特徴
          </div>
          <div class="card_row_body">
            <div class="simpleTag">焼き肉</div>
            <div class="simpleTag">広い店内</div>
          </div>
        </div>
        <div class="card_row">
          <div class="card_row_header">
            店舗の説明
          </div>
          <div class="card_row_body tagsWrapper">
            七輪焼き肉・安安は、美味しい焼肉を...
          </div>
        </div>
        <div class="card_row">
          <div class="card_row_header">
            住所
          </div>
          <div class="card_row_body">
            渋谷区桜丘町2-12 渋谷亀八ビル4F
          </div>
        </div>
        <div class="card_row">
          <div class="card_row_header">
            地図
          </div>
          <div class="card_row_body">
            <div class="location">
              <div class="location_map" data-latitude="41.40243" data-longitude="2.17551"></div>
            </div>
          </div>
        </div>
        <div class="card_row">
          <div class="card_row_header">
            電話番号
          </div>
          <div class="card_row_body">
            03-3464-0722
          </div>
        </div>
        <div class="card_row">
          <div class="card_row_header">
            営業時間
          </div>
          <div class="card_row_body">
            月〜木 17:00〜翌4:30\n金土日祝・祝前 16:00〜翌4:30
          </div>
        </div>
        <div class="card_row">
          <div class="card_row_header">
            定休日
          </div>
          <div class="card_row_body">
            元旦のみ
          </div>
        </div>
        <div class="card_row">
          <a class="btn outerBtn" href="http://www.fuji-tatsu.co.jp/">オフィシャルサイト</a>
        </div>
        <div class="card_row">
          <img src="http://s3.amasonaws.com/foo/bar" alt="七輪焼き肉・安安">
        </div>
      </div>
    </div>
  </body>
</html>
```

#### Request Parameter and its type

None.

#### Response and its type

This code is for explanation of the API response, so this is NOT the same HTML as production code.

* Model

    ```haskell
store :: Store  -- Data representing the store logging in
    ```
* Pseudo template file

    ```html
<!DOCTYPE html>
<html lang="ja">
  <head>
    <meta content="IE=edge" http-equiv="X-UA-Compatible">
    <meta content="text/html; charset=UTF-8" http-equiv="Content-Type">
    <meta content="width=device-width,initial-scale=1" name="viewport">
    <title>kucipong ストア管理</title>
    <link rel="stylesheet" href="/static/main.css">
  </head>
  <body>
    <div class="storeMenu">
      <a class="btn defaultBtn" href="/store/edit">ストア情報編集</a>
      <a class="btn outerBtn" href="/store/coupon">クーポン一覧</a>
    </div>
    <div class="storeBody card">
      <div class="storeBody_info_body card_body">
        <div class="card_row">
          <div class="card_row_header">
            店舗名
          </div>
          <div class="card_row_body">
            #{storeName store}
          </div>
        </div>
        <div class="card_row">
          <div class="card_row_header">
            カテゴリ
          </div>
          <div class="card_row_body">
            #{showStoreCategoryJa (storeAddress store)}
          </div>
        </div>
        <div class="card_row">
          <div class="card_row_header">
            店舗の特徴
          </div>
          <div class="card_row_body">
            #{{ for tag in storeTags store }}
            <div class="simpleTag">#{showStoreTagJa tag}</div>
            #{{ endfor }}
          </div>
        </div>
        <div class="card_row">
          <div class="card_row_header">
            店舗の説明
          </div>
          <div class="card_row_body tagsWrapper">
            #{storeDescription store}
          </div>
        </div>
        <div class="card_row">
          <div class="card_row_header">
            住所
          </div>
          <div class="card_row_body">
            #{storeAddress store}
          </div>
        </div>
        <div class="card_row">
          <div class="card_row_header">
            地図
          </div>
          <div class="card_row_body">
            <div class="location">
              <div class="location_map" data-latitude="#{latitude (storeArea store)}" data-longitude="#{longitude (storeArea store)}"></div>
            </div>
          </div>
        </div>
        <div class="card_row">
          <div class="card_row_header">
            電話番号
          </div>
          <div class="card_row_body">
            #{storePhoneNumber store}
          </div>
        </div>
        <div class="card_row">
          <div class="card_row_header">
            営業時間
          </div>
          <div class="card_row_body">
            #{storeHours store}
          </div>
        </div>
        <div class="card_row">
          <div class="card_row_header">
            定休日
          </div>
          <div class="card_row_body">
            #{storeCloseOn store}
          </div>
        </div>
        <div class="card_row">
          <a class="btn outerBtn" href="#{storeURL store}">オフィシャルサイト</a>
        </div>
        <div class="card_row">
          <img src="#{storeImage store}" alt="七輪焼き肉・安安">
        </div>
      </div>
    </div>
  </body>
</html>
```

### GET my store edit page

#### Sample request and response

This response is a sample simple HTML for convenience.
Some user may bookmark or share this URI and search engine also crawl this page, so do not include version number in the URI.

```bash
$ curl -G "http://$domain/store/edit" \
  -H "AUTH-TOKEN: ${token}"

<!DOCTYPE html>
<html lang="ja">
  <head>
    <meta content="IE=edge" http-equiv="X-UA-Compatible">
    <meta content="text/html; charset=UTF-8" http-equiv="Content-Type">
    <meta content="width=device-width,initial-scale=1" name="viewport">
    <title>kucipong ストア管理</title>
    <link rel="stylesheet" href="/static/main.css">
  </head>
  <body>
    <div class="storeBody card">
      <form class="storeBody_info_body card_body" enctype="multipart/form-data" method="POST">
        <div class="card_row">
          <label for="storeName" class="card_row_header">
            店舗名
          </label>
          <input id="storeName" name="storeName" type="text" class="card_input"
            value="七輪焼肉・安安">
        </div>
        <div class="card_row">
          <label for="storeCategory" class="card_row_header">
            カテゴリ
          </label>
          <select id="storeCategory" name="storeCategory" class="card_input" value="0">
            <option value="0">グルメ</option>
            <option value="1">ファッション</option>
            <option value="2">ガジェット</option>
            <option value="3">旅行</option>
            <option value="4">美容</option>
          </select>
        </div>
        <div class="card_row">
          <div class="card_row_header">
            店舗の特徴
          </div>
          <div class="card_row_body">
            <div class="checkboxWrapper">
              <input class="checkbox" type="checkbox" name="storeTags0" id="storeTags0" checked>
              <label for="storeTags0" class="checkboxLabel">
                焼き肉
              </label>
            </div>
            <div class="checkboxWrapper">
              <input class="checkbox" type="checkbox" name="storeTags1" id="storeTags1" checked>
              <label for="storeTags1" class="checkboxLabel">
                日本食
              </label>
            </div>
            <div class="checkboxWrapper">
              <input class="checkbox" type="checkbox" name="storeTags2" id="storeTags2" checked>
              <label for="storeTags2" class="checkboxLabel">
                寿司
              </label>
            </div>
            <div class="checkboxWrapper">
              <input class="checkbox" type="checkbox" name="storeTags3" id="storeTags3">
              <label for="storeTags3" class="checkboxLabel">
                広々とした店内
              </label>
            </div>
          </div>
        </div>
        <div class="card_row">
          <label for="storeDescription" class="card_row_header">
            店舗の説明
          </label>
          <textarea id="storeDescription" name="storeDescription" type="text" class="card_input">
            七輪焼き肉・安安は、美味しい焼肉を...
          </textarea>
        </div>
        <div class="card_row">
          <label for="storeAddress" class="card_row_header">
            住所
          </label>
          <input id="storeAddress" name="storeAddress" type="text" class="card_input"
            value="渋谷区桜丘町2-12 渋谷亀八ビル4F">
        </div>
        <div class="card_row">
          <div class="card_row_header">
            地図
          </div>
          <div class="card_row_body">
            <div class="location">
              <div class="location_map" data-latitude="41.40243" data-longitude="2.17551">
                <input type="hidden" name="latitude" value="41.40243">
                <input type="hidden" name="latitude" value="2.17551">
              </div>
            </div>
          </div>
        </div>
        <div class="card_row">
          <label for="storePhoneNumber" class="card_row_header">
            電話番号
          </label>
          <input id="storePhoneNumber" name="storePhoneNumber" type="text" class="card_input"
            value="03-3464-0722">
        </div>
        <div class="card_row">
          <label for="storeHours" class="card_row_header">
            営業時間
          </label>
          <input id="storeHours" name="storeHours" type="text" class="card_input"
            value="月〜木 17:00〜翌4:30\n金土日祝・祝前 16:00〜翌4:30">
        </div>
        <div class="card_row">
          <label for="storeCloseOn" class="card_row_header">
            定休日
          </label>
          <input id="storeCloseOn" name="storeCloseOn" type="text" class="card_input"
            value="元旦のみ">
        </div>
        <div class="card_row">
          <label for="storeURL" class="card_row_header">
            オフィシャルサイト
          </label>
          <input id="storeURL" name="storeURL" type="text" class="card_input"
            value="http://www.fuji-tatsu.co.jp/">
        </div>
        <div class="card_row">
          <label for="storeImage" class="card_row_header">
            店舗イメージ画像
          </label>
          <input id="storeImage" name="storeImage" type="file" class="card_input">
        </div>
        <div class="card_row submit_row">
          <button type="submit" class="btn outerBtn">この内容で店舗情報を更新する</button>
        </div>
      </form>
    </div>
  </body>
</html>
```

#### Request Parameter and its type

None.

#### Response and its type

This code is for explanation of the API response, so this is NOT the same HTML as production code.

* Model

    ```haskell
store :: Store  -- Data representing the store logging in
    ```
* Pseudo template file

    ```html
<!DOCTYPE html>
<html lang="ja">
  <head>
    <meta content="IE=edge" http-equiv="X-UA-Compatible">
    <meta content="text/html; charset=UTF-8" http-equiv="Content-Type">
    <meta content="width=device-width,initial-scale=1" name="viewport">
    <title>kucipong ストア管理</title>
    <link rel="stylesheet" href="/static/main.css">
  </head>
  <body>
    <div class="storeBody card">
      <form class="storeBody_info_body card_body" enctype="multipart/form-data" method="POST">
        <div class="card_row">
          <label for="storeName" class="card_row_header">
            店舗名
          </label>
          <input id="storeName" name="storeName" type="text" class="card_input"
            value="#{storeName store}">
        </div>
        <div class="card_row">
          <label for="storeCategory" class="card_row_header">
            カテゴリ
          </label>
          <select id="storeCategory" name="storeCategory" class="card_input" value="#{storeCategory store}">
            #{{ for category in storeCategoryList }}
            <option value="#{category}">#{showStoreCategoryJa category}</option>
            #{{ endfor }}
          </select>
        </div>
        <div class="card_row">
          <div class="card_row_header">
            店舗の特徴
          </div>
          <div class="card_row_body">
            #{{ for tag in storeTagList }}
            <div class="checkboxWrapper">
              <input class="checkbox" type="checkbox" name="storeTags#{tag}" id="storeTags#{tag}"
                #{{ if (tag `elem` storeTags store) }}
                checked
                #{{ endif }}
              >
              <label for="storeTags#{tag}" class="checkboxLabel">
                #{showStoreTagJa tag}
              </label>
            </div>
            #{{ endfor }}
          </div>
        </div>
        <div class="card_row">
          <label for="storeDescription" class="card_row_header">
            店舗の説明
          </label>
          <textarea id="storeDescription" name="storeDescription" type="text" class="card_input">
            #{storeDescription store}
          </textarea>
        </div>
        <div class="card_row">
          <label for="storeAddress" class="card_row_header">
            住所
          </label>
          <input id="storeAddress" name="storeAddress" type="text" class="card_input"
            value="#{storeAddress store}">
        </div>
        <div class="card_row">
          <div class="card_row_header">
            地図
          </div>
          <div class="card_row_body">
            <div class="location">
              <div class="location_map" data-latitude="#{latitude (storeArea store)}" data-longitude="#{longitude (storeArea store)}"></div>
                <input type="hidden" name="latitude" value="#{latitude (storeArea store)}">
                <input type="hidden" name="latitude" value="#{longitude (storeArea store)}">
            </div>
          </div>
        </div>
        <div class="card_row">
          <label for="storePhoneNumber" class="card_row_header">
            電話番号
          </label>
          <input id="storePhoneNumber" name="storePhoneNumber" type="text" class="card_input"
            value="#{storePhoneNumber store}">
        </div>
        <div class="card_row">
          <label for="storeHours" class="card_row_header">
            営業時間
          </label>
          <input id="storeHours" name="storeHours" type="text" class="card_input"
            value="#{storeHours store}">
        </div>
        <div class="card_row">
          <label for="storeCloseOn" class="card_row_header">
            定休日
          </label>
          <input id="storeCloseOn" name="storeCloseOn" type="text" class="card_input"
            value="#{storeCloseOn store}">
        </div>
        <div class="card_row">
          <label for="storeURL" class="card_row_header">
            オフィシャルサイト
          </label>
          <input id="storeURL" name="storeURL" type="text" class="card_input"
            value="#{storeURL store}">
        </div>
        <div class="card_row">
          <label for="storeImage" class="card_row_header">
            店舗イメージ画像
          </label>
          <input id="storeImage" name="storeImage" type="file" class="card_input">
        </div>
        <div class="card_row submit_row">
          <button type="submit" class="btn outerBtn">この内容で店舗情報を更新する</button>
        </div>
      </form>
    </div>
  </body>
</html>
```

### Edit my store information

#### Sample request and response

This response is a sample simple HTML for convenience.
Some user may bookmark or share this URI and search engine also crawl this page, so do not include version number in the URI.

```bash
$ curl -X POST "http://$domain/store/edit" \
  -H "AUTH-TOKEN: ${token}" \
  -F "storeName=七輪焼き肉・安安 \
  -F "storeCategory=0 \
  -F "storeLatitude=41.40243 \
  -F "storeLongitude=2.17551 \
  -F "storeTags0=on \
  -F "storeTags1=on \
  -F "storeTags3=on \
  -F "storeDescription=七輪焼き肉・安安は、美味しい焼肉を... \
  -F "storeImage=http://s3.amasonaws.com/foo/baz \
  -F "storeAddress=渋谷区桜丘町2-12 渋谷亀八ビル4F \
  -F "storePhoneNumber=03-3464-0722 \
  -F "storeHours=月〜木 17:00〜翌4:30\n金土日祝・祝前 16:00〜翌4:30 \
  -F "storeCloseOn=元旦のみ \
  -F "storeURL=url=http://www.fuji-tatsu.co.jp

(Same response as "GET my store information")

```

#### Request Parameter and its type

Basically same as the `Store` type except following points.

* `storeArea` is given by independent properties "storeLatitude" and "storeLongitude"
* you can check whether a n-th tag is an element of the `storeTags` list by checking the property "storeTags#{n}" is "on" or null

#### Response and its type

Same as the response of "GET my shop information" API,
except that the requested parameters are set as the store data.

### GET my store coupons

#### Sample request and response

This response is a sample simple HTML for convenience.
Some user may bookmark or share this URI and search engine also crawl this page, so do not include version number in the URI.

```bash
$ curl -G "http://$domain/store/coupon" \
  -H "AUTH-TOKEN: ${token}"

<!DOCTYPE html>
<html lang="ja">
  <head>
    <meta content="IE=edge" http-equiv="X-UA-Compatible">
    <meta content="text/html; charset=UTF-8" http-equiv="Content-Type">
    <meta content="width=device-width,initial-scale=1" name="viewport">
    <title>kucipong クーポン管理</title>
    <link rel="stylesheet" href="/static/main.css">
  </head>
  <body>
    <div class="storeMenu">
      <a class="btn outerBtn" href="/store/coupon/create">クーポン追加</a>
      <a class="btn defaultBtn" href="/store">ストア情報</a>
    </div>
    <div class="coupon">
      <div class="card">
        <div class="card_header">
          <h2 class="card_header_text">七輪焼肉・安安</h2>
          <div class="card_header_icon"><a class="icon-edit" href="/store/coupon/3/edit"></a></div>
        </div>
        <div class="card_image">
          <img src="http://s3.amasonaws.com/foo/bar" alt="七輪焼き肉・安安">
        </div>
        <div class="card_body">
          <div class="card_body_title">
            当日OK! 21時以降のご予約で2.5H飲放題付き料理4品で3,600円
          </div>
          <div class="card_body_summary">
            <span class="card_body_summary-sub">10% OFF</span>
            <span class="card_body_summary-main">3600円</span>
          </div>
        </div>
      </div>
      <div class="card">
        <div class="card_header">
          <h2 class="card_header_text">七輪焼肉・安安</h2>
          <div class="card_header_icon"><a class="icon-edit" href="/store/coupon/3/edit"></a></div>
        </div>
        <div class="card_image">
          <img src="http://s3.amasonaws.com/foo/bar" alt="七輪焼き肉・安安">
        </div>
        <div class="card_body">
          <div class="card_body_title">
            当日OK! 21時以降のご予約で2.5H飲放題付き料理4品で3,600円
          </div>
          <div class="card_body_summary">
            <span class="card_body_summary-sub">10% OFF</span>
            <span class="card_body_summary-main">3600円</span>
          </div>
        </div>
      </div>
    </div>
  </body>
</html>
```

#### Request Parameter and its type

None.

#### Response and its type

This code is for explanation of the API response, so this is NOT the same HTML as production code.

* Model

    ```haskell
store :: Store      -- Data representing the store logging in
coupons :: [Coupon] -- All coupons that store has
    ```
* Pseudo template file

    ```html
<!DOCTYPE html>
<html lang="ja">
  <head>
    <meta content="IE=edge" http-equiv="X-UA-Compatible">
    <meta content="text/html; charset=UTF-8" http-equiv="Content-Type">
    <meta content="width=device-width,initial-scale=1" name="viewport">
    <title>kucipong クーポン管理</title>
    <link rel="stylesheet" href="/static/main.css">
  </head>
  <body>
    <div class="storeMenu">
      <a class="btn outerBtn" href="/store/coupon/create">クーポン追加</a>
      <a class="btn defaultBtn" href="/store">ストア情報</a>
    </div>
    <div class="coupon">
      <div class="card">
        #{{ for coupon in coupons }}
        <div class="card_header">
          <h2 class="card_header_text">#{storeName (couponStore coupon)}</h2>
          <div class="card_header_icon"><a class="icon-edit" href="/store/coupon/#{couponId coupon}/edit"></a></div>
        </div>
        <div class="card_image">
          <img src="#{couponImage coupon}" alt="#{storeName (couponStore coupon)}">
        </div>
        <div class="card_body">
          <div class="card_body_title">
            #{couponTitle coupon}
          </div>

          <div class="card_body_summary">
            #{{ if (couponType == CouponDiscount) then }}
            <span class="card_body_summary-sub">#{discountMinimumFee coupon}円以上のお買い上げで</span>
            <span class="card_body_summary-main">#{discountRate coupon}% OFF</span>

            #{{ elseif (couponType == CouponGift) then }}
            <span class="card_body_summary-sub">#{giftMinimumFee coupon}円以上のお買い上げで</span>
            <span class="card_body_summary-main">#{maybe "非売品" ((<> "円相当の品") . tshow) (giftReferencePrice coupon)} をプレゼント</span>

            #{{ elseif (couponType == CouponSet) then }}
            <span class="card_body_summary-sub">#{100 - (setPrice coupon * 100 `div` setReferencePrice coupon)}% OFF</span>
            <span class="card_body_summary-main">#{setPrice coupon}円</span>

            #{{ elseif (couponType == CouponOther) then }}
            <span class="card_body_summary-main">#{otherContent coupon}</span>

            #{{ endif }}
          </div>
        </div>
      </div>
      #{{ endfor }}
    </div>
  </body>
</html>
```

### GET my coupon create page

#### Sample request and response

This response is a sample simple HTML for convenience.
Some user may bookmark or share this URI and search engine also crawl this page, so do not include version number in the URI.

```bash
$ curl -G "http://$domain/store/coupon/create" \
  -H "AUTH-TOKEN: ${token}"

<!DOCTYPE html>
<html lang="ja">
  <head>
    <meta content="IE=edge" http-equiv="X-UA-Compatible">
    <meta content="text/html; charset=UTF-8" http-equiv="Content-Type">
    <meta content="width=device-width,initial-scale=1" name="viewport">
    <title>kucipong クーポン追加</title>
    <link rel="stylesheet" href="/static/main.css">
  </head>
  <body>
    <div class="storeMenu">
      <a class="btn outerBtn" href="/store/coupon">クーポン一覧</a>
      <a class="btn defaultBtn" href="/store">ストア情報</a>
    </div>

    <div class="storeBody card">
      <form class="storeBody_info_body card_body" enctype="multipart/form-data" method="POST">
        <div class="card_row">
          <label for="couponTitle" class="card_row_header">
            タイトル
          </label>
          <input id="couponTitle" name="couponTitle" type="text" class="card_input"
            value="">
        </div>
        <div class="card_row">
          <label for="couponImage" class="card_row_header">
            クーポンイメージ画像
          </label>
          <input id="couponImage" name="couponImage" type="file" accept="image/*" class="card_input">
        </div>
        <div class="card_row">
          <label for="couponValidFrom" class="card_row_header">
            クーポン有効期限（利用開始日）
          </label>
          <input id="couponValidFrom" name="couponValidFrom" type="date" class="card_input">
        </div>
        <div class="card_row">
          <label for="couponExpire" class="card_row_header">
            クーポン有効期限（利用期限）
          </label>
          <input id="couponExpire" name="couponExpire" type="date" class="card_input">
        </div>
        <div class="card_row">
          <label for="couponType" class="card_row_header">
            クーポン種別
          </label>
          <select id="couponType" name="couponType" class="card_input" value="0">
            <option value="0">購入額から割引</option>
            <option value="1">購入者プレゼント</option>
            <option value="2">特別セット</option>
            <option value="3">その他クーポン</option>
          </select>
        </div>
        <div class="js-couponDiscount">
          <div class="card_row">
            <label for="discountRate" class="card_row_header">
              割引率 (ご購入総額からxx% OFF)
            </label>
            <input id="discountRate" name="discountRate" type="number" class="card_input">
          </div>
          <div class="card_row">
            <label for="discountMinimumFee" class="card_row_header">
              最低購入額 (xx円以上お買い上げのお客様のみ割引)
            </label>
            <input id="discountMinimumFee" name="discountMinimumFee" type="number" class="card_input">
          </div>
          <div class="card_row">
            <label for="discountOtherConditions" class="card_row_header">
              その他のクーポン適用条件
            </label>
            <input id="discountOtherConditions" name="discountOtherConditions" type="text" class="card_input">
          </div>
        </div>
        <div class="js-couponGift">
          <div class="card_row">
            <label for="giftContent" class="card_row_header">
              プレゼント内容
            </label>
            <input id="giftContent" name="giftContent" type="text" class="card_input">
          </div>
          <div class="card_row">
            <label for="giftReferencePrice" class="card_row_header">
              プレゼント参考価格 (非売品の場合は記入しないでください)
            </label>
            <input id="giftReferencePrice" name="giftReferencePrice" type="number" class="card_input">
          </div>
          <div class="card_row">
            <label for="giftMinimumFee" class="card_row_header">
              最低購入額 (xx円以上お買い上げのお客様にプレゼント)
            </label>
            <input id="giftMinimumFee" name="giftMinimumFee" type="number" class="card_input">
          </div>
          <div class="card_row">
            <label for="giftOtherConditions" class="card_row_header">
              その他のクーポン適用条件
            </label>
            <input id="giftOtherConditions" name="giftOtherConditions" type="text" class="card_input">
          </div>
        </div>
        <div class="js-couponSet">
          <div class="card_row">
            <label for="setContent" class="card_row_header">
              セット内容
            </label>
            <input id="setContent" name="setContent" type="text" class="card_input">
          </div>
          <div class="card_row">
            <label for="setPrice" class="card_row_header">
              セット価格
            </label>
            <input id="setPrice" name="setPrice" type="number" class="card_input">
          </div>
          <div class="card_row">
            <label for="setReferencePrice" class="card_row_header">
              セット参考価格 (非売品の場合は記入しないでください)
            </label>
            <input id="setReferencePrice" name="setReferencePrice" type="number" class="card_input">
          </div>
          <div class="card_row">
            <label for="setOtherConditions" class="card_row_header">
              その他のクーポン適用条件
            </label>
            <input id="setOtherConditions" name="setOtherConditions" type="text" class="card_input">
          </div>
        </div>
        <div class="js-couponOther">
          <div class="card_row">
            <label for="otherContent" class="card_row_header">
              クーポン内容
            </label>
            <input id="otherContent" name="otherContent" type="text" class="card_input">
          </div>
          <div class="card_row">
            <label for="otherConditions" class="card_row_header">
              クーポン適用条件
            </label>
            <input id="otherConditions" name="otherConditions" type="text" class="card_input">
          </div>
        </div>
        <div class="card_row submit_row">
          <button type="submit" class="btn outerBtn">この内容でクーポンを作成</button>
        </div>
      </form>
    </div>
  </body>
```

#### Request Parameter and its type

None.

#### Response and its type

See the sample response.

### POST my new coupon

#### Sample request and response

This response is a sample simple HTML for convenience.
Some user may bookmark or share this URI and search engine also crawl this page, so do not include version number in the URI.

```bash
$ curl -X POST "http://$domain/store/coupon/create" \
  -H "AUTH-TOKEN: ${token}" \
  -F "couponTitle=当日OK! 21時以降のご予約で2.5H飲放題付き料理4品で3,600円" \
  -F "couponImage=@/path/to/image" \
  -F "couponValidFrom=2016-04-15" \
  -F "couponExpire=2016-04-30" \
  -F "couponType=2" \
  -F "discountRate=null" \
  -F "discountMinimumFee=null" \
  -F "discountOtherConditions=null" \
  -F "giftContent=null" \
  -F "giftReferencePrice=null" \
  -F "giftMinimumFee=null" \
  -F "giftOtherConditions=null" \
  -F "setContent=2.5時間飲放題付きの料理4品です。 1. 前菜 2. 焼き肉盛り合わせ 3. 冷麺 4. アイスクリーム" \
  -F "setPrice=3600" \
  -F "setReferencePrice=4000" \
  -F "setOtherConditions=※ 1グループ1回のご利用時の利用枚数制限はありません。※ 21時以降のご予約のお客様が対象です。" \
  -F "otherContent=null" \
  -F "otherConditions=null"

(Same response as "GET my store coupons")
```

#### Request Parameter and its type

Basically it's same as `Coupon` data type except that it doesn't have `couponStore` field.

#### Response and its type

Same as the response of "GET my store coupons".

### GET my coupon edit page

#### Sample request and response

This response is a sample simple HTML for convenience.
Some user may bookmark or share this URI and search engine also crawl this page, so do not include version number in the URI.

```bash
$ curl -G "http://$domain/store/coupon/${coupon_id}/edit" \
  -H "AUTH-TOKEN: ${token}"

<!DOCTYPE html>
<html lang="ja">
  <head>
    <meta content="IE=edge" http-equiv="X-UA-Compatible">
    <meta content="text/html; charset=UTF-8" http-equiv="Content-Type">
    <meta content="width=device-width,initial-scale=1" name="viewport">
    <title>kucipong クーポン編集</title>
    <link rel="stylesheet" href="/static/main.css">
  </head>
  <body>
    <div class="storeMenu">
      <a class="btn outerBtn" href="/store/coupon">クーポン一覧</a>
      <a class="btn defaultBtn" href="/store">ストア情報</a>
    </div>

    <div class="storeBody card">
      <form class="storeBody_info_body card_body" enctype="multipart/form-data" method="POST">
        <div class="card_row">
          <label for="couponTitle" class="card_row_header">
            タイトル
          </label>
          <input id="couponTitle" name="couponTitle" type="text" class="card_input"
            value="当日OK! 21時以降のご予約で2.5H飲放題付き料理4品で3,600円">
        </div>
        <div class="card_row">
          <label for="couponImage" class="card_row_header">
            クーポンイメージ画像
          </label>
          <input id="couponImage" name="couponImage" type="file" accept="image/*" class="card_input">
        </div>
        <div class="card_row">
          <label for="couponValidFrom" class="card_row_header">
            クーポン有効期限（利用開始日）
          </label>
          <input id="couponValidFrom" name="couponValidFrom" type="date" class="card_input" value="2016-04-15">
        </div>
        <div class="card_row">
          <label for="couponExpire" class="card_row_header">
            クーポン有効期限（利用期限）
          </label>
          <input id="couponExpire" name="couponExpire" type="date" class="card_input" value="2016-04-30">
        </div>
        <div class="card_row">
          <label for="couponType" class="card_row_header">
            クーポン種別
          </label>
          <select id="couponType" name="couponType" class="card_input" value="2">
            <option value="0">購入額から割引</option>
            <option value="1">購入者プレゼント</option>
            <option value="2">特別セット</option>
            <option value="3">その他クーポン</option>
          </select>
        </div>
        <div class="js-couponDiscount">
          <div class="card_row">
            <label for="discountRate" class="card_row_header">
              割引率 (ご購入総額からxx% OFF)
            </label>
            <input id="discountRate" name="discountRate" type="number" class="card_input">
          </div>
          <div class="card_row">
            <label for="discountMinimumFee" class="card_row_header">
              最低購入額 (xx円以上お買い上げのお客様のみ割引)
            </label>
            <input id="discountMinimumFee" name="discountMinimumFee" type="number" class="card_input">
          </div>
          <div class="card_row">
            <label for="discountOtherConditions" class="card_row_header">
              その他のクーポン適用条件
            </label>
            <input id="discountOtherConditions" name="discountOtherConditions" type="text" class="card_input">
          </div>
        </div>
        <div class="js-couponGift">
          <div class="card_row">
            <label for="giftContent" class="card_row_header">
              プレゼント内容
            </label>
            <input id="giftContent" name="giftContent" type="text" class="card_input">
          </div>
          <div class="card_row">
            <label for="giftReferencePrice" class="card_row_header">
              プレゼント参考価格 (非売品の場合は記入しないでください)
            </label>
            <input id="giftReferencePrice" name="giftReferencePrice" type="number" class="card_input">
          </div>
          <div class="card_row">
            <label for="giftMinimumFee" class="card_row_header">
              最低購入額 (xx円以上お買い上げのお客様にプレゼント)
            </label>
            <input id="giftMinimumFee" name="giftMinimumFee" type="number" class="card_input">
          </div>
          <div class="card_row">
            <label for="giftOtherConditions" class="card_row_header">
              その他のクーポン適用条件
            </label>
            <input id="giftOtherConditions" name="giftOtherConditions" type="text" class="card_input">
          </div>
        </div>
        <div class="js-couponSet">
          <div class="card_row">
            <label for="setContent" class="card_row_header">
              セット内容
            </label>
            <input id="setContent" name="setContent" type="text" class="card_input" value="2.5時間飲放題付きの料理4品です。1. 前菜 2. 焼き肉盛り合わせ 3. 冷麺 4. アイスクリーム">
          </div>
          <div class="card_row">
            <label for="setPrice" class="card_row_header">
              セット価格
            </label>
            <input id="setPrice" name="setPrice" type="number" class="card_input" value="3600">
          </div>
          <div class="card_row">
            <label for="setReferencePrice" class="card_row_header">
              セット参考価格 (非売品の場合は記入しないでください)
            </label>
            <input id="setReferencePrice" name="setReferencePrice" type="number" class="card_input" value="4000">
          </div>
          <div class="card_row">
            <label for="setOtherConditions" class="card_row_header">
              その他のクーポン適用条件
            </label>
            <input id="setOtherConditions" name="setOtherConditions" type="text" class="card_input" value="※ 1グループ1回のご利用時の利用枚数制限はありません。\n※ 21時以降のご予約のお客様が対象です。">
          </div>
        </div>
        <div class="js-couponOther">
          <div class="card_row">
            <label for="otherContent" class="card_row_header">
              クーポン内容
            </label>
            <input id="otherContent" name="otherContent" type="text" class="card_input">
          </div>
          <div class="card_row">
            <label for="otherConditions" class="card_row_header">
              クーポン適用条件
            </label>
            <input id="otherConditions" name="otherConditions" type="text" class="card_input">
          </div>
        </div>
        <div class="card_row submit_row">
          <button type="submit" class="btn outerBtn">この内容でクーポンを更新</button>
        </div>
      </form>
    </div>
  </body>
```

#### Request Parameter and its type

None.

#### Response and its type

This code is for explanation of the API response, so this is NOT the same HTML as production code.

* Model

    ```haskell
coupon :: Coupon -- The coupon to edit
    ```
* Pseudo template file

    ```html
<!DOCTYPE html>
<html lang="ja">
  <head>
    <meta content="IE=edge" http-equiv="X-UA-Compatible">
    <meta content="text/html; charset=UTF-8" http-equiv="Content-Type">
    <meta content="width=device-width,initial-scale=1" name="viewport">
    <title>kucipong クーポン編集</title>
    <link rel="stylesheet" href="/static/main.css">
  </head>
  <body>
    <div class="storeMenu">
      <a class="btn outerBtn" href="/store/coupon">クーポン一覧</a>
      <a class="btn defaultBtn" href="/store">ストア情報</a>
    </div>

    <div class="storeBody card">
      <form class="storeBody_info_body card_body" enctype="multipart/form-data" method="POST">
        <div class="card_row">
          <label for="couponTitle" class="card_row_header">
            タイトル
          </label>
          <input id="couponTitle" name="couponTitle" type="text" class="card_input"
            value="#{couponTitle coupon}">
        </div>
        <div class="card_row">
          <label for="couponImage" class="card_row_header">
            クーポンイメージ画像
          </label>
          <input id="couponImage" name="couponImage" type="file" accept="image/*" class="card_input">
        </div>
        <div class="card_row">
          <label for="couponValidFrom" class="card_row_header">
            クーポン有効期限（利用開始日）
          </label>
          <input id="couponValidFrom" name="couponValidFrom" type="date" class="card_input" value="#{couponValidFrom coupon}">
        </div>
        <div class="card_row">
          <label for="couponExpire" class="card_row_header">
            クーポン有効期限（利用期限）
          </label>
          <input id="couponExpire" name="couponExpire" type="date" class="card_input" value="#{couponExpire couponType}">
        </div>
        <div class="card_row">
          <label for="couponType" class="card_row_header">
            クーポン種別
          </label>
          <select id="couponType" name="couponType" class="card_input" value="#{couponType coupon}">
            {{ for ctype in couponTypeList }}
            <option value="#{ctype}">#{showCouponTypeJa ctype}</option>
            {{ endfor }}
          </select>
        </div>
        <div class="js-couponDiscount">
          <div class="card_row">
            <label for="discountRate" class="card_row_header">
              割引率 (ご購入総額からxx% OFF)
            </label>
            <input id="discountRate" name="discountRate" type="number" class="card_input" value="{discountRate coupon}">
          </div>
          <div class="card_row">
            <label for="discountMinimumFee" class="card_row_header">
              最低購入額 (xx円以上お買い上げのお客様のみ割引)
            </label>
            <input id="discountMinimumFee" name="discountMinimumFee" type="number" class="card_input" value="#{discountMinimumFee coupon}">
          </div>
          <div class="card_row">
            <label for="discountOtherConditions" class="card_row_header">
              その他のクーポン適用条件
            </label>
            <input id="discountOtherConditions" name="discountOtherConditions" type="text" class="card_input" value="#{discountOtherConditions coupon}">
          </div>
        </div>
        <div class="js-couponGift">
          <div class="card_row">
            <label for="giftContent" class="card_row_header">
              プレゼント内容
            </label>
            <input id="giftContent" name="giftContent" type="text" class="card_input" value="#{giftContent coupon}">
          </div>
          <div class="card_row">
            <label for="giftReferencePrice" class="card_row_header">
              プレゼント参考価格 (非売品の場合は記入しないでください)
            </label>
            <input id="giftReferencePrice" name="giftReferencePrice" type="number" class="card_input" value="#{giftReferencePrice coupon}">
          </div>
          <div class="card_row">
            <label for="giftMinimumFee" class="card_row_header">
              最低購入額 (xx円以上お買い上げのお客様にプレゼント)
            </label>
            <input id="giftMinimumFee" name="giftMinimumFee" type="number" class="card_input" value="#{giftMinimumFee coupon}">
          </div>
          <div class="card_row">
            <label for="giftOtherConditions" class="card_row_header">
              その他のクーポン適用条件
            </label>
            <input id="giftOtherConditions" name="giftOtherConditions" type="text" class="card_input" value="#{giftOtherConditions coupon}">
          </div>
        </div>
        <div class="js-couponSet">
          <div class="card_row">
            <label for="setContent" class="card_row_header">
              セット内容
            </label>
            <input id="setContent" name="setContent" type="text" class="card_input" value="#{setContent coupon}">
          </div>
          <div class="card_row">
            <label for="setPrice" class="card_row_header">
              セット価格
            </label>
            <input id="setPrice" name="setPrice" type="number" class="card_input" value="#{setPrice coupon}">
          </div>
          <div class="card_row">
            <label for="setReferencePrice" class="card_row_header">
              セット参考価格 (非売品の場合は記入しないでください)
            </label>
            <input id="setReferencePrice" name="setReferencePrice" type="number" class="card_input" value="#{setReferencePrice coupon}">
          </div>
          <div class="card_row">
            <label for="setOtherConditions" class="card_row_header">
              その他のクーポン適用条件
            </label>
            <input id="setOtherConditions" name="setOtherConditions" type="text" class="card_input" value="#{setOtherConditions coupon}">
          </div>
        </div>
        <div class="js-couponOther">
          <div class="card_row">
            <label for="otherContent" class="card_row_header">
              クーポン内容
            </label>
            <input id="otherContent" name="otherContent" type="text" class="card_input" value="#{otherContent coupon}">
          </div>
          <div class="card_row">
            <label for="otherConditions" class="card_row_header">
              クーポン適用条件
            </label>
            <input id="otherConditions" name="otherConditions" type="text" class="card_input" value="#{otherConditions coupon}">
          </div>
        </div>
        <div class="card_row submit_row">
          <button type="submit" class="btn outerBtn">この内容でクーポンを更新</button>
        </div>
      </form>
    </div>
  </body>
</html>
```

### Edit my coupon

#### Sample request and response

This response is a sample simple HTML for convenience.
Some user may bookmark or share this URI and search engine also crawl this page, so do not include version number in the URI.

```bash
$ curl -X POST "http://$domain/store/coupon/${coupon_id}/edit" \
  -H "AUTH-TOKEN: ${token}" \
  -F "couponTitle=当日OK! 21時以降のご予約で2.5H飲放題付き料理4品で3,600円" \
  -F "couponImage=@/path/to/image" \
  -F "couponValidFrom=2016-04-15" \
  -F "couponExpire=2016-04-30" \
  -F "couponType=2" \
  -F "discountRate=null" \
  -F "discountMinimumFee=null" \
  -F "discountOtherConditions=null" \
  -F "giftContent=null" \
  -F "giftReferencePrice=null" \
  -F "giftMinimumFee=null" \
  -F "giftOtherConditions=null" \
  -F "setContent=2.5時間飲放題付きの料理4品です。 1. 前菜 2. 焼き肉盛り合わせ 3. 冷麺 4. アイスクリーム" \
  -F "setPrice=3600" \
  -F "setReferencePrice=4000" \
  -F "setOtherConditions=※ 1グループ1回のご利用時の利用枚数制限はありません。※ 21時以降のご予約のお客様が対象です。" \
  -F "otherContent=null" \
  -F "otherConditions=null"

(Same response as "GET my store coupons")
```

#### Request Parameter and its type

Basically it's same as `Coupon` data type except that it doesn't have `couponStore` field.

#### Response and its type

Same as the response of "GET my store coupons".
