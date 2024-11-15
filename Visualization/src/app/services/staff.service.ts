import { Injectable } from '@angular/core';
import { MessageService } from './message.service';
import { HttpClient, HttpHeaders } from '@angular/common/http';

@Injectable({
  providedIn: 'root'
})
export class StaffService {
  theServerURL = 'https://localhost:5001/api/Patient';
  httpOptions = {
    headers: new HttpHeaders({'Content-Type': 'application/json' })
  };
  
  constructor(private messageService: MessageService, private http: HttpClient) { }

  private log(message: string) {
    this.messageService.add(`${message}`);
  }
}
