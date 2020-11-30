import json
import requests
from urllib import parse

from os import listdir
from os.path import isfile, join

def bind_json(folder, files):
    result = []
    for file_name in files:
        single_json = json.load(open('./input/' + folder + file_name, 'r', encoding='utf8'))
        result += single_json
    return result

def getLatLng(text):
    filter_string = ['제주', 'Jeju', '제주 Jeju', 'Jeju 제주', '제주 Jeju Island', 'Jeju Island 제주',
                     '제주도', 'Jeju Island', '제주도 Jeju', 'Jeju 제주도', '제주도 Jeju Island', 'Jeju Island 제주도',
                     '서귀포', 'Seogwipo', '서귀포 Seogwipo', 'Seogwipo 서귀포']
    if text in filter_string:
        return None

    url = 'https://dapi.kakao.com/v2/local/search/keyword.json?query=' + parse.quote_plus(text)
    headers = {"Authorization": "KakaoAK d9696e52861ec16d2f264fd9bc2deab2"}
    result = json.loads(str(requests.get(url, headers=headers).text))

    for matches in result['documents']:
        if matches['address_name'].split()[0] == '제주특별자치도' and matches['place_name'] != '제주도':
            return {'address': matches['address_name'], 'x': float(matches['x']), 'y': float(matches['y'])}

    return None

if __name__ == '__main__':
    output_files = [f for f in listdir('./input/output/') if isfile(join('./input/output', f))]
    output_bind = bind_json('output/', output_files)
    with open('./output/output.json', 'w', encoding='utf8') as output_file:
        json.dump(output_bind, output_file, ensure_ascii=False)

    hashtag_files = [f for f in listdir('./input/hashtags/') if isfile(join('./input/hashtags', f))]
    hashtag_bind = bind_json('hashtags/', hashtag_files)
    with open('./output/hashtags.json', 'w', encoding='utf8') as output_file:
        json.dump(hashtag_bind, output_file, ensure_ascii=False)

    key_files = [f for f in listdir('input/keys/') if isfile(join('input/keys', f))]
    key_bind = bind_json('keys/', key_files)
    with open('./output/keys.json', 'w', encoding='utf8') as output_file:
        json.dump(key_bind, output_file, ensure_ascii=False)

    location_files = [f for f in listdir('./input/locations/') if isfile(join('./input/locations', f))]
    location_bind = bind_json('locations/', location_files)

    for location in location_bind[:]:
        location_result = getLatLng(location['location'])
        if location_result is not None:
            location['address'] = location_result['address']
            location['x'] = location_result['x']
            location['y'] = location_result['y']
        else:
            location_bind.remove(location)

    with open('./output/locations.json', 'w', encoding='utf8') as output_file:
        json.dump(location_bind, output_file, ensure_ascii=False)
