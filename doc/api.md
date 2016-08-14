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
